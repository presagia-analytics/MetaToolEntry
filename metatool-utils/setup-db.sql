CREATE TABLE trial(trial_id STRING, 
                   user_name STRING,
                   group_name STRING,
                   nickname STRING, 
                   disease STRING, 
                   line STRING,
                   phase STRING
                   );

CREATE TABLE publication(trial_id STRING, 
                         user_name STRING, 
                         group_name STRING, 
                         doi STRING,
                         year INTEGER,
                         first_name STRING,
                         last_name STRING,
                         article STRING); 

CREATE TYPE surv_time_type AS ENUM ('month', 'week', 'year', 'day');

CREATE TYPE surv_type AS ENUM ('os', 'pfs');

CREATE TABLE survival_outcome(trial_id STRING,
                              outcome_id STRING,
                              user_name STRING,
                              group_name STRING,
                              survival_type surv_type,
                              time_unit surv_time_type,
                              outcome_row_id STRING,
                              treatment STRING,
                              subgroup STRING,
                              pathology STRING,
                              n INTEGER,
                              num_events INTEGER,
                              est_median DOUBLE,
                              est_95_ci_upper DOUBLE,
                              est_95_ci_lower DOUBLE,
                              fup_median DOUBLE,
                              hazard_ratio DOUBLE,
                              hr_95_ci_upper DOUBLE,
                              hr_95_ci_lower DOUBLE);

CREATE TABLE survival_curve(outcome_row_id STRING, time DOUBLE, surv DOUBLE);

CREATE TABLE survival_figures(outcome_id STRING,
                              uploaded STRING, 
                              generated STRING, 
                              generated_ipd STRING); 

CREATE TABLE continuous_outcome(trial_id STRING,
                                outcome_id STRING,
                                user_name STRING,
                                group_name STRING,
                                outcome_names STRING,
                                treatment STRING,
                                subgroup STRING,
                                pathology STRING,
                                val DOUBLE);

CREATE TYPE cat_type AS ENUM ('RECIST', 'Resp');

CREATE TABLE categorical_outcome(trial_id STRING, 
                                 outcome_id STRING,
                                 user_name STRING,
                                 group_name STRING,
                                 outcome_names STRING,
                                 levels STRING,
                                 ordered BOOLEAN,
                                 treatment STRING,
                                 subgroup STRING,
                                 pathology STRING,
                                 n INTEGER,
                                 val STRING);

