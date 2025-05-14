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

keydates.Rout: keydates.R 
	$(pipeR)

flows.Rout: flows.R params.rda
	$(pipeR)

spec.Rout: spec.R params.rda flows.rda
	$(pipeR)

## Are we even using other reporting fractions? If not, then we need to delete the ones we don't use.

timevar_spec.Rout: timevar_spec.R spec.rds keydates.rda ../data/rp_eligfrac3.csv params.rda
	$(pipeR)

seroprevdata.Rout: seroprevdata.R keydates.rda ../data/serop_avgcase_data.csv    
	$(pipeR)

# eligfrac3.calibrate.Rout: calibrate.R
%.calibrate.Rout: calibrate.R %.timevar_spec.rds seroprevdata.rds params.rda keydates.rda timevar_spec.R
	$(pipeR)

plot_model_fit.Rout: plot_model_fit.R ../data/serop_avgcase_data.csv keydates.rda params.rda eligfrac3.calibrate.rds
	$(pipeR)

# eligfrac2.calibrate_plot.Rout: calibrate_plot.R
plot_true_infections.Rout: plot_true_infections.R ../data/true_infections_data.csv seroprevdata.rds keydates.rda params.rda
	$(pipeR)

beta_plot.Rout: beta_plot.R ../data/true_infections_data.csv
	$(pipeR)

counterfact_plot.Rout: counterfact_plot.R ../data/true_infections_data.csv ../data/rp_eligfrac2.csv seroprevdata.rds keydates.rda params.rda
	$(pipeR)

allscenarios.Rout: allscenarios.R ../data/serop_avgcase_data.csv ../data/true_infections_data.csv ../data/eligibility_adjusted_cases.csv
	$(pipeR)

eff_Rt.Rout: eff_Rt.R ../data/true_infections_data.csv
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
