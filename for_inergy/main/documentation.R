qué estoy entregando?

1)  
- "monthly_characteristic_generation"   
()

3)
- "matrix_allocation_mean" daily mean coefficients matrix
(matrix_)

- "mean_allocated_community" overall mean for the whole community
(REC_mean)
- "mean_allocated_participant..." overall mean for participant ...
(individual_mean_participant_)  

- "monthly_allocated_participant..._DayWeek" monthly facets for the participant ... (DayWeek)
- "                                _WeekEnd" monthly facets for the participant ... (WeekEnd)
(community_participant)

- "allocation_" allocation according to payback and investment (the price curve is missing) 
(final)

overall)
- "comparison_different_selections&allocations" compare statistics with sustainable and profitable REC 



df_months_stats = dplyr::bind_rows(list_months_stats, .id = "month")


