## This is NL Omicron

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

### Makestuff

Sources += Makefile
Sources += $(wildcard *.R) notes.md

vacdat.Rout: vacdat.R ../data/vaccination-coverage-map.csv 
	$(pipeRcall)

params.Rout: params.R vacdat.rda
	$(pipeRcall)

flows.Rout: flows.R params.rda
	$(pipeRcall)

## Con't refactoring here next time

timevar_spec.Rout: timevar_spec.R flows.rda vacdat.rda params.rda
	$(pipeRcall)

seroprevdata.Rout: seroprevdata.R params.rda ../data/raw_citf_data.csv    
	$(pipeRcall)

fitsero.Rout: fitsero.R ../data/raw_citf_data.csv params.rda
	$(pipeRcall)

calibrate.Rout: calibrate.R timevar_spec.rds seroprevdata.rds fitsero.rds params.rda
	$(pipeR)

bettas.Rout: bettas.R seroprevdata.rds fitsero.rds params.rda calibrate.rds
	$(pipeR)

reprod_numb.Rout: reprod_numb.R fitsero.rds params.rda calibrate.rds
	$(pipeR)
	
als_R0.Rout: als_R0.R fitsero.rds params.rda calibrate.rds
	$(pipeR)

plot_model_fit.Rout: plot_model_fit.R seroprevdata.rds fitsero.rds params.rda calibrate.rds
	$(pipeR)

true_vs_reported_plot.Rout: true_vs_reported_plot.R calibrate.rds fitsero.rds ../data/serop_avgcase_data.csv params.rda 
	$(pipeR)

stack_transmission_R0_plot.Rout: stack_transmission_R0_plot.R ../figures/als_R0.png ../figures/reprod_numb.png
	$(pipeR)

timeline.Rout: timeline.R 
	$(pipeR)

#######################################################################

## Revision 1

R1_seroprevdata.Rout: R1_seroprevdata.R params.rda ../data/raw_citf_data.csv    
	$(pipeRcall)

R1_fitsero.Rout: R1_fitsero.R R1_seroprevdata.rds params.rda
	$(pipeRcall)

R1_timevar_spec.Rout: R1_timevar_spec.R flows.rda vacdat.rda params.rda
	$(pipeRcall)

muzeta.Rout: muzeta.R params.rda
	$(pipeRcall)

pipeRimplicit += newspecs
# muzeta.newspecs.Rout: newspecs.R
muzeta.newspecs.Rout: newspecs.R R1_timevar_spec.rds muzeta.rda
	$(pipeRcall)

R1_calibrate.Rout: R1_calibrate.R R1_timevar_spec.rds R1_seroprevdata.rds R1_fitsero.rds params.rda
	$(pipeRcall)

pipeRimplicit += R1_calibrate
# R1_muzeta.calibrate.Rout: R1_calibrate.R
R1_muzeta.calibrate.Rout: R1_calibrate.R muzeta.newspecs.rds R1_seroprevdata.rds R1_fitsero.rds params.rda
	$(pipeRcall)

R1_grid_calibrate.Rout: R1_grid_calibrate.R muzeta.newspecs.rds R1_seroprevdata.rds R1_fitsero.rds params.rda
	$(pipeRcall)

R1_grid_params.Rout: R1_grid_params.R R1_grid_calibrate.rds
	$(pipeRcall)

R1_grid_params_plot.Rout: R1_grid_params_plot.R R1_grid_params.rds
	$(pipeRcall)

R1_grid_betas.Rout: R1_grid_betas.R R1_grid_calibrate.rds vacdat.rda
	$(pipeRcall)

R1_grid_inc.Rout: R1_grid_inc.R R1_grid_calibrate.rds R1_fitsero.rds vacdat.rda
	$(pipeRcall)

pipeRimplicit += R1_bettas.Rout
# R1_muzeta.bettas.Rout: R1_bettas.R
R1_muzeta.bettas.Rout: R1_bettas.R R1_seroprevdata.rds R1_fitsero.rds params.rda R1_muzeta.calibrate.rds
	$(pipeRcall)

R1_grid_effreprod.Rout: R1_grid_effreprod.R R1_grid_calibrate.rds vacdat.rda
	$(pipeRcall)

R1_effreprod.Rout: R1_effreprod.R R1_seroprevdata.rds R1_fitsero.rds params.rda R1_calibrate.rds
	$(pipeRcall)
	
R1_true_vs_reported_plot.Rout: R1_true_vs_reported_plot.R R1_calibrate.rds R1_fitsero.rds ../data/serop_avgcase_data.csv params.rda 
	$(pipeR)

R1_als_R0.Rout: R1_als_R0.R R1_fitsero.rds params.rda R1_calibrate.rds
	$(pipeR)

R1_plot_model_fit.Rout: R1_plot_model_fit.R R1_seroprevdata.rds R1_fitsero.rds params.rda R1_calibrate.rds
	$(pipeR)

R1_reprod_numb.Rout: R1_reprod_numb.R R1_fitsero.rds params.rda R1_calibrate.rds
	$(pipeR)
	
R1_sensitivity_plot.Rout: R1_sensitivity_plot.R R1_grid_params.rds R1_grid_calibrate.rds params.rda
	$(pipeRcall)
	
	
R1_prior_posterior_plot.Rout: R1_prior_posterior_plot.R R1_grid_calibrate.rds params.rda
	$(pipeRcall)

R1_timeline.Rout: R1_timeline.R 
	$(pipeR)

## SUPP

supp1.Rout: supp1.R
	$(pipeR)

sublineages.Rout: sublineages.R
	$(pipeR)

sirtest.Rout: sirtest.R
	$(pipeR)

Ignore += makestuff
msrepo = https://github.com/dushoff

Makefile: makestuff/00.stamp
makestuff/%.stamp:
	- $(RM) makestuff/*.stamp
	(cd makestuff && $(MAKE) pull) || git clone --depth 1 $(msrepo)/makestuff
	touch $@

-include makestuff/os.mk

-include makestuff/pipeR.mk

-include makestuff/git.mk
-include makestuff/visual.mk
