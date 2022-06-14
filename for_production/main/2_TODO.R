#### define n_community_max #### 

# TODO: recalculate this n_community, should not be max, should be mean
# should always use summer months to calculate the community max
df_gen_energy_original = df_gen$energy
df_gen$energy = df_gen_energy_original*1.5
# df_gen = df_gen[, c(1, 3)]
df_gen_sunny = df_gen[df_local_time$sunny, 2]
n_community_max = calculate_n_community_max(generation = df_gen$energy, consumption = df_cons[, c(1:ncol(df_cons))])

n_community_max = 8

global_investment = max(df_gen$energy, na.rm = T)*1100

# TODO: should change the curve of df_gen_sunny because it has a very strange pattern in summer
p <- ggplot() +
  geom_line(aes(x = 1:length(df_gen_sunny), y = df_gen_sunny)) 
