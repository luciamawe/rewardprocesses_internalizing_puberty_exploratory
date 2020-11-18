#Lmer syntax in case we need it again 

confirmatory4_compositestriatum_excludingoutliers <- lme4::lmer(cbcl_scr_syn_internal_r ~ 
                                                                  pds_p_ss_category*
                                                                  striatum_rvsn_ant_z+
                                                                  race.ethnicity.5level +
                                                                  demo_race_hispanic +
                                                                  high.educ +
                                                                  household.income +
                                                                  married.or.livingtogether +
                                                                  interview_age+
                                                                  #  fam_history_q6d_depression + #added mother's depression
                                                                  (1 | site_id_l/rel_family_id),  #same thing as site is the same thing as (1|site) + (1|family:site), this is what done in paper by Paulus et al. (2019), 22 sites 
                                                                #data = subset(PDS_correct, striatum_rvsn_ant_z > -3 & striatum_rvsn_ant_z < 3 & cbcl_scr_syn_internal_r_z > -3 & cbcl_scr_syn_internal_r_z < 3))
                                                                data = tempdata)
summary(confirmatory4_compositestriatum_excludingoutliers)