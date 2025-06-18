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

## Where is the data for this? 
timevar_spec.Rout: timevar_spec.R flows.rda vacdat.Rout.csv params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R ../data/raw_citf_data.csv    
	$(pipeR)

calibrate.Rout: calibrate.R timevar_spec.rds seroprevdata.rds params.rda timevar_spec.R
	$(pipeRcall)

plot_model_fit.Rout: plot_model_fit.R ../data/serop_avgcase_data.csv params.rda calibrate.rds
	$(pipeR)

prevalence_plot.Rout: prevalence_plot.R ../data/serop_avgcase_data.csv params.rda calibrate.rds
	$(pipeR)

beta_plot.Rout: beta_plot.R ../outputs/true_infections_data.csv
	$(pipeR)

true_vs_reported_plot.Rout: true_vs_reported_plot.R ../data/serop_avgcase_data.csv ../outputs/true_infections_data.csv 
	$(pipeR)

eff_Rt.Rout: eff_Rt.R ../outputs/true_infections_data.csv
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
