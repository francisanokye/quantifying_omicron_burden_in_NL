## This is [project.Makefile] …

current: target
-include target.mk
Ignore = target.mk

# -include makestuff/perl.def

vim_session:
	bash -cl "vmt"

######################################################################

### Makestuff

Sources += Makefile
Sources += $(wildcard *.R)

params.Rout: params.R
	$(pipeR)

flows.Rout: flows.R params.rda
	$(pipeR)

vacdat.Rout: vacdat.R ../data/vaccination-coverage-map.csv
	$(pipeR)

## This is using serop only
timevar_spec.Rout: timevar_spec.R flows.rda vacdat.rds params.rda
	$(pipeR)

## This is using sero_inc
# timevar_spec_inc.Rout: timevar_spec_inc.R flows.R
timevar_spec_inc.Rout: timevar_spec_inc.R flows.rda vacdat.rds params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R ../data/raw_citf_data.csv    
	$(pipeR)

fitsero.Rout: fitsero.R ../data/raw_citf_data.csv params.rda
	$(pipeR)

calibrate.Rout: calibrate.R timevar_spec.rds seroprevdata.rds params.rda
	$(pipeRcall)

# calibrate_inc.Rout: calibrate_inc.R timevar_spec_inc.R
calibrate_inc.Rout: calibrate_inc.R timevar_spec_inc.rds fitsero.rds params.rda
	$(pipeRcall)

output.Rout: output.R calibrate_inc.rds
	$(pipeR)

plot_model_fit.Rout: plot_model_fit.R fitsero.rds params.rda calibrate_inc.rds
	$(pipeR)

prevalence_plot.Rout: prevalence_plot.R params.rda calibrate_inc.rds
	$(pipeR)

beta_plot.Rout: beta_plot.R calibrate_inc.rds
	$(pipeR)

true_vs_reported_plot.Rout: true_vs_reported_plot.R calibrate_inc.rds ../data/serop_avgcase_data.csv #../outputs/true_infections_data.csv 
	$(pipeR)

eff_Rt.Rout: eff_Rt.R calibrate_inc.rds #output.rds
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
