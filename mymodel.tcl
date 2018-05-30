output -log_file mymodel.iclo 
set_default_model_dichtomous 3PL 
options -default_prior_b none
options -default_prior_a {lognormal 0.0 0.5}
options -default_prior_c {beta 5 17 0.0 1.0}
options -D 1.0
allocate_items_dist 26 -num_latent_dist_points 21 
read_examinees mymodel.dat 26i1 
starting_values_dichotomous
EM_steps -max_iter 2000
write_item_param mymodel.iclp 
write_latent_dist mymodel.icld 
release_items_dist
