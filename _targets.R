library(targets)
library(tarchetypes)
#library(covidGrandeRegion)
#library(dplyr)
##library(lubridate)
##library(tidyr)
#library(ggplot2)
##library(data.table)
##library(janitor)
##library(stringr)
#library(modeltime)
#library(timetk)
#library(tidymodels)

source("functions/functions.R")
options(clustermq.scheduler = "multicore")

tar_option_set(
  packages = c(
    "covidGrandeRegion",
    "geojsonio",
    "sf",
    "colorspace", 
    "dplyr",
    "rlang", 
    "data.table",
    "lubridate",
    "ggplot2",
    "modeltime",
    "timetk",
    "tidymodels",
    "DALEX",
    "DALEXtra",
    "kableExtra"
  )
)

list(
  tar_target(
    raw_weekly_data,
    get_greater_region_data(daily = FALSE),
    format = "qs"
  ),

  tar_target(
    population_data,
    get_population_data(),
    format = "fst"
  ),

  tar_target(
    grande_region_map,
    get_grande_region_map(),
    format = "qs"
  ),

  tar_target(
    normalized_monthly_data,
    get_normalized_monthly_data(raw_weekly_data, population_data),
    format = "fst"
  ),

  tar_target(
    epidem_map,
    plot_epidem_map(normalized_monthly_data, grande_region_map),
    format = "qs"
  ),

  tar_target(
    normalized_weekly_data,
    normalize_weekly_data(raw_weekly_data, population_data),
    format = "fst"
  ),

  tar_target(
    mobility_raw,
    fread("data/2020_LU_Region_Mobility_Report_2021_03_05.csv"),
    format = "fst"
  ),

  tar_target(
    mobility,
    prep_mobility(mobility_raw),
    format = "fst"
  ),


  tar_target(
    plot_mobility,
    make_plot_mobility(mobility),
    format = "qs"
  ),

  tar_target(
    epid_curves,
    plot_epidem_curve(normalized_weekly_data),
    format = "qs"
  ),

  tar_target(
    data_for_model,
    prep_data_for_model(normalized_weekly_data, mobility),
    format = "fst"
  ),

  tar_target(
    save_data_for_model,
    write_data_for_model(data_for_model),
    format = "file"
  ),

  tar_target(
    splits,
    time_series_split(data_for_model, date_var = week, assess = "9 weeks", cumulative = TRUE),
    format = "qs"
  ),

  tar_target(
    cv_plan,
    view_cv_plan(splits),
    format = "qs"
  ),

  tar_target(
    model_fit_prophet_boost,
    setup_prophet_boost_model(),
    format = "qs"
  ),

  tar_target(
    model_fit_arima_boost,
    setup_arima_boost_model(),
    format = "qs"
  ),

  tar_target(
    model_fit_arima_boost_tuned,
    setup_arima_boost_model(mtry = tune(),
                             tree = tune(),
                             learn_rate = tune(),
                             tree_depth = tune()),
    format = "qs"
  ),

  tar_target(
    arima_boost_grid,
    {model_fit_arima_boost_tuned %>%
       parameters() %>%
       finalize(select(data_for_model, -Luxembourg)) %>%
       grid_max_entropy(size = 5)
    },
    format = "qs"
  ),

  # remove recipe and lag the variables before, this will avoid issues with DALEX
  tar_target(
    recipe_spec,
    setup_recipe_spec(Luxembourg ~ ., splits),
    format = "qs"
  ),

  tar_target(
    prepped_training_data,
    view_prepped_training_data(recipe_spec),
    format = "qs"
  ),

  tar_target(
    workflow_fit_prophet_boost,
    setup_workflow(model_fit_prophet_boost, recipe_spec),
    format = "qs"
  ),

  tar_target(
    workflow_fit_arima_boost,
    setup_workflow(model_fit_arima_boost, recipe_spec),
    format = "qs"
  ),

  tar_target(
    workflow_fit_arima_boost_tuned,
    setup_workflow(model_fit_arima_boost_tuned, recipe_spec),
    format = "qs"
  ),

  tar_target(
    fitted_prophet_boost,
    fit(workflow_fit_prophet_boost, training(splits)),
    format = "qs"
  ),

  tar_target(
    fitted_arima_boost,
    fit(workflow_fit_arima_boost, training(splits)),
    format = "qs"
  ),

  tar_target(
    cv_splits,
    time_series_cv(training(splits), initial = 36, assess = 6, lag = 4, cumulative = TRUE),
    format = "qs"
  ),

  tar_target(
    fitted_arima_boost_tuned,
    tune_grid(workflow_fit_arima_boost_tuned,
              resamples = cv_splits,
              grid = arima_boost_grid),
    format = "qs"
  ),

  tar_target(
    best_arima_boost_params,
    show_best(fitted_arima_boost_tuned, n = 1),
    format = "qs"
  ),

  tar_target(
    best_model_fit_arima_boost, 
    setup_arima_boost_model(mtry = best_arima_boost_params$mtry,
                            trees = best_arima_boost_params$trees,
                            tree_depth = best_arima_boost_params$tree_depth,
                            learn_rate = best_arima_boost_params$learn_rate),
    format = "qs"
  ),

  tar_target(
    workflow_fit_best_arima_boost,
    setup_workflow(best_model_fit_arima_boost, recipe_spec),
    format = "qs"
  ),

  tar_target(
    fitted_best_arima_boost,
    fit(workflow_fit_best_arima_boost, training(splits)),
    format = "qs"
  ),

  tar_target(
    model_table,
    modeltime_table(#fitted_prophet_boost,
                    fitted_arima_boost),
                    #fitted_best_arima_boost),
    format = "qs"
  ),

  tar_target(
    calibrated_wf,
    modeltime_calibrate(model_table, new_data = testing(splits)),
    format = "qs"
  ),

  tar_target(
    forecast_plot,
    view_forecast(calibrated_wf, data_for_model, splits),
    format = "qs"
  ),

 # see issue:https://github.com/tidymodels/workflows/issues/63
  tar_target(
    explain_arima_boost,
    explain_tidymodels(fitted_arima_boost,
                       data = training(splits),
                       y = training(splits)$Luxembourg),
    format = "qs"
  ),

  tar_target(
    var_imp,
    variable_importance(explain_arima_boost),
    format = "qs"
  ),

  tar_target(
    plot_var_imp,
    plot(var_imp),
    format = "qs"
  ),


  tar_target(
    lag_belgique_3_response,
    model_profile(explain_arima_boost, variables = "lag_Belgique_03", type = "partial"),
    format = "qs"
  ),

  tar_target(
    plot_var_resp,
    plot(lag_belgique_3_response),
    format = "qs"
  ),

  tar_target(
    pred_contributions,
    predict_parts_break_down(explain_arima_boost, new_observation = testing(splits)[1,]),
    format = "qs"
  ),

  tar_target(
    plot_pred_contributions,
    plot(pred_contributions),
    format = "qs"
  ),

# take a look at break down plots 
# https://pbiecek.github.io/breakDown/reference/break_down.html

  tar_render(
    paper,
    "paper/paper.Rmd"
  )
)

#splits <- dataset %>%
#  time_series_split(date_var = week, assess = "1 month", cumulative = TRUE)
#
#splits %>%
#  tk_time_series_cv_plan() %>%
#  plot_time_series_cv_plan(week, gd_luxembourg, .interactive = FALSE)
#
#model_fit_prophet_boost <- prophet_boost(seasonality_daily = FALSE,
#                           seasonality_weekly = TRUE,
#                           seasonality_yearly = FALSE) %>%  
#  set_engine("prophet_xgboost")
#
#
#model_fit_arima_boost <- arima_boost() %>%
#  set_engine("arima_xgboost")
#
#recipe_spec <- recipe(gd_luxembourg ~ ., training(splits)) %>%
#  #step_timeseries_signature(week) %>%
#  #step_rm(contains("am.pm"), contains("hour"), contains("minute"),
#  #        contains("second"), contains("xts")) %>%   
#  step_fourier(week, period = 365, K = 3)
#
#recipe_spec %>% prep() %>% juice() 
#
#workflow_fit_prophet_boost <- workflow() %>%
#  add_model(model_fit_prophet_boost) %>%
#  add_recipe(recipe_spec) 
#
#fitted_prophet_boost <- fit(workflow_fit_prophet_boost, training(splits))
#
#workflow_fit_arima_boost <- workflow() %>%
#  add_model(model_fit_arima_boost) %>%
#  add_recipe(recipe_spec) 
#
#fitted_arima_boost <- fit(workflow_fit_arima_boost, training(splits))
#
#model_table <- modeltime_table(
#  fitted_prophet_boost,
#  fitted_arima_boost
#)
#
#calibrated_wf <- modeltime_calibrate(model_table,
#                                     new_data = testing(splits))
#
#calibrated_wf %>%
#  modeltime_forecast(actual_data = dataset, new_data = testing(splits)) %>%
#  plot_modeltime_forecast()

# old
# for explainability 
  #tar_target(
  #  iml_fitted_best_arima_boost,
  #  fit(best_model_fit_arima_boost, Luxembourg ~ ., data = prepped_training_data),
  #  format = "qs"
  #),

  #tar_target(
  #  trained_recipe,
  #  prep(recipe_spec, training = training(splits)),
  #  format = "qs"
  #),

  #tar_target(
  #  test_bake_features,
  #  select(bake(trained_recipe, testing(splits)), -Luxembourg),
  #  format = "qs"
  #),

  #tar_target(
  #  target_var,
  #  select(testing(splits), Luxembourg),
  #  format = "qs"
  #),

  ##problem, IML doesn't like date variables
  #tar_target(
  #  predictor,
  #  Predictor$new(
  #              model = iml_fitted_best_arima_boost,
  #              data = test_bake_features,
  #              y = target_var,
  #              predict.fun = predict_wrapper2 
  #            ),
  #  format = "qs"
  #),

  #tar_target(
  #  feature_importance,
  #  FeatureImp$new(predictor, loss = "ce"),
  #  format = "qs"
  #),

  #tar_target(
  #  plot_feature_importance,
  #  plot(feature_importance),
  #  format = "qs"
  #),
