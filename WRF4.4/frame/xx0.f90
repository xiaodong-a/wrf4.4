!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit. Your changes to this file will be lost.
!
SUBROUTINE nl_set_gsfcrad_gocart_coupling ( id_id , gsfcrad_gocart_coupling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gsfcrad_gocart_coupling
  INTEGER id_id
  model_config_rec%gsfcrad_gocart_coupling = gsfcrad_gocart_coupling
  RETURN
END SUBROUTINE nl_set_gsfcrad_gocart_coupling
SUBROUTINE nl_set_gsfcgce_gocart_coupling ( id_id , gsfcgce_gocart_coupling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gsfcgce_gocart_coupling
  INTEGER id_id
  model_config_rec%gsfcgce_gocart_coupling = gsfcgce_gocart_coupling
  RETURN
END SUBROUTINE nl_set_gsfcgce_gocart_coupling
SUBROUTINE nl_set_auxinput12_inname ( id_id , auxinput12_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: auxinput12_inname
  INTEGER id_id
  model_config_rec%auxinput12_inname = trim(auxinput12_inname)
  RETURN
END SUBROUTINE nl_set_auxinput12_inname
SUBROUTINE nl_set_io_form_auxinput12 ( id_id , io_form_auxinput12 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: io_form_auxinput12
  INTEGER id_id
  model_config_rec%io_form_auxinput12 = io_form_auxinput12
  RETURN
END SUBROUTINE nl_set_io_form_auxinput12
SUBROUTINE nl_set_emi_inname ( id_id , emi_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: emi_inname
  INTEGER id_id
  model_config_rec%emi_inname = trim(emi_inname)
  RETURN
END SUBROUTINE nl_set_emi_inname
SUBROUTINE nl_set_fireemi_inname ( id_id , fireemi_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: fireemi_inname
  INTEGER id_id
  model_config_rec%fireemi_inname = trim(fireemi_inname)
  RETURN
END SUBROUTINE nl_set_fireemi_inname
SUBROUTINE nl_set_input_chem_inname ( id_id , input_chem_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: input_chem_inname
  INTEGER id_id
  model_config_rec%input_chem_inname = trim(input_chem_inname)
  RETURN
END SUBROUTINE nl_set_input_chem_inname
SUBROUTINE nl_set_emi_outname ( id_id , emi_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: emi_outname
  INTEGER id_id
  model_config_rec%emi_outname = trim(emi_outname)
  RETURN
END SUBROUTINE nl_set_emi_outname
SUBROUTINE nl_set_fireemi_outname ( id_id , fireemi_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: fireemi_outname
  INTEGER id_id
  model_config_rec%fireemi_outname = trim(fireemi_outname)
  RETURN
END SUBROUTINE nl_set_fireemi_outname
SUBROUTINE nl_set_input_chem_outname ( id_id , input_chem_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: input_chem_outname
  INTEGER id_id
  model_config_rec%input_chem_outname = trim(input_chem_outname)
  RETURN
END SUBROUTINE nl_set_input_chem_outname
SUBROUTINE nl_set_io_style_emissions ( id_id , io_style_emissions )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: io_style_emissions
  INTEGER id_id
  model_config_rec%io_style_emissions = io_style_emissions
  RETURN
END SUBROUTINE nl_set_io_style_emissions
SUBROUTINE nl_set_bioemdt ( id_id , bioemdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: bioemdt
  INTEGER id_id
  model_config_rec%bioemdt(id_id) = bioemdt
  RETURN
END SUBROUTINE nl_set_bioemdt
SUBROUTINE nl_set_photdt ( id_id , photdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: photdt
  INTEGER id_id
  model_config_rec%photdt(id_id) = photdt
  RETURN
END SUBROUTINE nl_set_photdt
SUBROUTINE nl_set_chemdt ( id_id , chemdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: chemdt
  INTEGER id_id
  model_config_rec%chemdt(id_id) = chemdt
  RETURN
END SUBROUTINE nl_set_chemdt
SUBROUTINE nl_set_ne_area ( id_id , ne_area )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ne_area
  INTEGER id_id
  model_config_rec%ne_area = ne_area
  RETURN
END SUBROUTINE nl_set_ne_area
SUBROUTINE nl_set_kemit ( id_id , kemit )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kemit
  INTEGER id_id
  model_config_rec%kemit = kemit
  RETURN
END SUBROUTINE nl_set_kemit
SUBROUTINE nl_set_nmegan ( id_id , nmegan )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nmegan
  INTEGER id_id
  model_config_rec%nmegan = nmegan
  RETURN
END SUBROUTINE nl_set_nmegan
SUBROUTINE nl_set_kfuture ( id_id , kfuture )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kfuture
  INTEGER id_id
  model_config_rec%kfuture = kfuture
  RETURN
END SUBROUTINE nl_set_kfuture
SUBROUTINE nl_set_kfire ( id_id , kfire )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kfire
  INTEGER id_id
  model_config_rec%kfire = kfire
  RETURN
END SUBROUTINE nl_set_kfire
SUBROUTINE nl_set_kemit_aircraft ( id_id , kemit_aircraft )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kemit_aircraft
  INTEGER id_id
  model_config_rec%kemit_aircraft = kemit_aircraft
  RETURN
END SUBROUTINE nl_set_kemit_aircraft
SUBROUTINE nl_set_kdvel ( id_id , kdvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kdvel
  INTEGER id_id
  model_config_rec%kdvel = kdvel
  RETURN
END SUBROUTINE nl_set_kdvel
SUBROUTINE nl_set_ndepvel ( id_id , ndepvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ndepvel
  INTEGER id_id
  model_config_rec%ndepvel = ndepvel
  RETURN
END SUBROUTINE nl_set_ndepvel
SUBROUTINE nl_set_kdepvel ( id_id , kdepvel )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kdepvel
  INTEGER id_id
  model_config_rec%kdepvel = kdepvel
  RETURN
END SUBROUTINE nl_set_kdepvel
SUBROUTINE nl_set_biomass_emiss_opt ( id_id , biomass_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: biomass_emiss_opt
  INTEGER id_id
  model_config_rec%biomass_emiss_opt(id_id) = biomass_emiss_opt
  RETURN
END SUBROUTINE nl_set_biomass_emiss_opt
SUBROUTINE nl_set_cam_mam_mode ( id_id , cam_mam_mode )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cam_mam_mode
  INTEGER id_id
  model_config_rec%cam_mam_mode = cam_mam_mode
  RETURN
END SUBROUTINE nl_set_cam_mam_mode
SUBROUTINE nl_set_cam_mam_nspec ( id_id , cam_mam_nspec )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cam_mam_nspec
  INTEGER id_id
  model_config_rec%cam_mam_nspec = cam_mam_nspec
  RETURN
END SUBROUTINE nl_set_cam_mam_nspec
SUBROUTINE nl_set_cam_mp_mam_cpled ( id_id , cam_mp_mam_cpled )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: cam_mp_mam_cpled
  INTEGER id_id
  model_config_rec%cam_mp_mam_cpled = cam_mp_mam_cpled
  RETURN
END SUBROUTINE nl_set_cam_mp_mam_cpled
SUBROUTINE nl_set_mozart_ph_diag ( id_id , mozart_ph_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mozart_ph_diag
  INTEGER id_id
  model_config_rec%mozart_ph_diag = mozart_ph_diag
  RETURN
END SUBROUTINE nl_set_mozart_ph_diag
SUBROUTINE nl_set_megan_specifier ( id_id , megan_specifier )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: megan_specifier
  INTEGER id_id
  model_config_rec%megan_specifier(id_id) = megan_specifier
  RETURN
END SUBROUTINE nl_set_megan_specifier
SUBROUTINE nl_set_megan_factors_file ( id_id , megan_factors_file )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: megan_factors_file
  INTEGER id_id
  model_config_rec%megan_factors_file = trim(megan_factors_file)
  RETURN
END SUBROUTINE nl_set_megan_factors_file
SUBROUTINE nl_set_megan_mapped_emisfctrs ( id_id , megan_mapped_emisfctrs )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: megan_mapped_emisfctrs
  INTEGER id_id
  model_config_rec%megan_mapped_emisfctrs = megan_mapped_emisfctrs
  RETURN
END SUBROUTINE nl_set_megan_mapped_emisfctrs
SUBROUTINE nl_set_vprm_opt ( id_id , vprm_opt )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: vprm_opt
  INTEGER id_id
  model_config_rec%vprm_opt(id_id) = vprm_opt
  RETURN
END SUBROUTINE nl_set_vprm_opt
SUBROUTINE nl_set_wpeat ( id_id , wpeat )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: wpeat
  INTEGER id_id
  model_config_rec%wpeat = wpeat
  RETURN
END SUBROUTINE nl_set_wpeat
SUBROUTINE nl_set_wflood ( id_id , wflood )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: wflood
  INTEGER id_id
  model_config_rec%wflood = wflood
  RETURN
END SUBROUTINE nl_set_wflood
SUBROUTINE nl_set_term_opt ( id_id , term_opt )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: term_opt
  INTEGER id_id
  model_config_rec%term_opt(id_id) = term_opt
  RETURN
END SUBROUTINE nl_set_term_opt
SUBROUTINE nl_set_do_pvozone ( id_id , do_pvozone )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: do_pvozone
  INTEGER id_id
  model_config_rec%do_pvozone = do_pvozone
  RETURN
END SUBROUTINE nl_set_do_pvozone
SUBROUTINE nl_set_chem_conv_tr ( id_id , chem_conv_tr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: chem_conv_tr
  INTEGER id_id
  model_config_rec%chem_conv_tr(id_id) = chem_conv_tr
  RETURN
END SUBROUTINE nl_set_chem_conv_tr
SUBROUTINE nl_set_conv_tr_wetscav ( id_id , conv_tr_wetscav )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: conv_tr_wetscav
  INTEGER id_id
  model_config_rec%conv_tr_wetscav(id_id) = conv_tr_wetscav
  RETURN
END SUBROUTINE nl_set_conv_tr_wetscav
SUBROUTINE nl_set_conv_tr_aqchem ( id_id , conv_tr_aqchem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: conv_tr_aqchem
  INTEGER id_id
  model_config_rec%conv_tr_aqchem(id_id) = conv_tr_aqchem
  RETURN
END SUBROUTINE nl_set_conv_tr_aqchem
SUBROUTINE nl_set_chem_opt ( id_id , chem_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: chem_opt
  INTEGER id_id
  model_config_rec%chem_opt(id_id) = chem_opt
  RETURN
END SUBROUTINE nl_set_chem_opt
SUBROUTINE nl_set_gaschem_onoff ( id_id , gaschem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gaschem_onoff
  INTEGER id_id
  model_config_rec%gaschem_onoff(id_id) = gaschem_onoff
  RETURN
END SUBROUTINE nl_set_gaschem_onoff
SUBROUTINE nl_set_aerchem_onoff ( id_id , aerchem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aerchem_onoff
  INTEGER id_id
  model_config_rec%aerchem_onoff(id_id) = aerchem_onoff
  RETURN
END SUBROUTINE nl_set_aerchem_onoff
SUBROUTINE nl_set_wetscav_onoff ( id_id , wetscav_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: wetscav_onoff
  INTEGER id_id
  model_config_rec%wetscav_onoff(id_id) = wetscav_onoff
  RETURN
END SUBROUTINE nl_set_wetscav_onoff
SUBROUTINE nl_set_dustwd_onoff ( id_id , dustwd_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dustwd_onoff
  INTEGER id_id
  model_config_rec%dustwd_onoff(id_id) = dustwd_onoff
  RETURN
END SUBROUTINE nl_set_dustwd_onoff
SUBROUTINE nl_set_cldchem_onoff ( id_id , cldchem_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cldchem_onoff
  INTEGER id_id
  model_config_rec%cldchem_onoff(id_id) = cldchem_onoff
  RETURN
END SUBROUTINE nl_set_cldchem_onoff
SUBROUTINE nl_set_is_full_tuv ( id_id , is_full_tuv )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: is_full_tuv
  INTEGER id_id
  model_config_rec%is_full_tuv(id_id) = is_full_tuv
  RETURN
END SUBROUTINE nl_set_is_full_tuv
SUBROUTINE nl_set_lambda_cutoff ( id_id , lambda_cutoff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: lambda_cutoff
  INTEGER id_id
  model_config_rec%lambda_cutoff(id_id) = lambda_cutoff
  RETURN
END SUBROUTINE nl_set_lambda_cutoff
SUBROUTINE nl_set_cld_od_opt ( id_id , cld_od_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cld_od_opt
  INTEGER id_id
  model_config_rec%cld_od_opt(id_id) = cld_od_opt
  RETURN
END SUBROUTINE nl_set_cld_od_opt
SUBROUTINE nl_set_pht_cldfrc_opt ( id_id , pht_cldfrc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: pht_cldfrc_opt
  INTEGER id_id
  model_config_rec%pht_cldfrc_opt(id_id) = pht_cldfrc_opt
  RETURN
END SUBROUTINE nl_set_pht_cldfrc_opt
SUBROUTINE nl_set_phot_blcld ( id_id , phot_blcld )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: phot_blcld
  INTEGER id_id
  model_config_rec%phot_blcld(id_id) = phot_blcld
  RETURN
END SUBROUTINE nl_set_phot_blcld
SUBROUTINE nl_set_vertmix_onoff ( id_id , vertmix_onoff )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: vertmix_onoff
  INTEGER id_id
  model_config_rec%vertmix_onoff(id_id) = vertmix_onoff
  RETURN
END SUBROUTINE nl_set_vertmix_onoff
SUBROUTINE nl_set_chem_in_opt ( id_id , chem_in_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: chem_in_opt
  INTEGER id_id
  model_config_rec%chem_in_opt(id_id) = chem_in_opt
  RETURN
END SUBROUTINE nl_set_chem_in_opt
SUBROUTINE nl_set_phot_opt ( id_id , phot_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: phot_opt
  INTEGER id_id
  model_config_rec%phot_opt(id_id) = phot_opt
  RETURN
END SUBROUTINE nl_set_phot_opt
SUBROUTINE nl_set_gas_drydep_opt ( id_id , gas_drydep_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gas_drydep_opt
  INTEGER id_id
  model_config_rec%gas_drydep_opt(id_id) = gas_drydep_opt
  RETURN
END SUBROUTINE nl_set_gas_drydep_opt
SUBROUTINE nl_set_aer_drydep_opt ( id_id , aer_drydep_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_drydep_opt
  INTEGER id_id
  model_config_rec%aer_drydep_opt(id_id) = aer_drydep_opt
  RETURN
END SUBROUTINE nl_set_aer_drydep_opt
SUBROUTINE nl_set_diagnostic_chem ( id_id , diagnostic_chem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: diagnostic_chem
  INTEGER id_id
  model_config_rec%diagnostic_chem(id_id) = diagnostic_chem
  RETURN
END SUBROUTINE nl_set_diagnostic_chem
SUBROUTINE nl_set_aero_diag_opt ( id_id , aero_diag_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aero_diag_opt
  INTEGER id_id
  model_config_rec%aero_diag_opt(id_id) = aero_diag_opt
  RETURN
END SUBROUTINE nl_set_aero_diag_opt
SUBROUTINE nl_set_aero_cw_diag_opt ( id_id , aero_cw_diag_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aero_cw_diag_opt
  INTEGER id_id
  model_config_rec%aero_cw_diag_opt(id_id) = aero_cw_diag_opt
  RETURN
END SUBROUTINE nl_set_aero_cw_diag_opt
SUBROUTINE nl_set_kfcup_diag ( id_id , kfcup_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kfcup_diag
  INTEGER id_id
  model_config_rec%kfcup_diag(id_id) = kfcup_diag
  RETURN
END SUBROUTINE nl_set_kfcup_diag
SUBROUTINE nl_set_aer_aerodynres_opt ( id_id , aer_aerodynres_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_aerodynres_opt
  INTEGER id_id
  model_config_rec%aer_aerodynres_opt(id_id) = aer_aerodynres_opt
  RETURN
END SUBROUTINE nl_set_aer_aerodynres_opt
SUBROUTINE nl_set_emiss_opt ( id_id , emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: emiss_opt
  INTEGER id_id
  model_config_rec%emiss_opt(id_id) = emiss_opt
  RETURN
END SUBROUTINE nl_set_emiss_opt
SUBROUTINE nl_set_emiss_opt_vol ( id_id , emiss_opt_vol )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: emiss_opt_vol
  INTEGER id_id
  model_config_rec%emiss_opt_vol(id_id) = emiss_opt_vol
  RETURN
END SUBROUTINE nl_set_emiss_opt_vol
SUBROUTINE nl_set_dust_opt ( id_id , dust_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_opt
  INTEGER id_id
  model_config_rec%dust_opt = dust_opt
  RETURN
END SUBROUTINE nl_set_dust_opt
SUBROUTINE nl_set_dust_schme ( id_id , dust_schme )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_schme
  INTEGER id_id
  model_config_rec%dust_schme = dust_schme
  RETURN
END SUBROUTINE nl_set_dust_schme
SUBROUTINE nl_set_dmsemis_opt ( id_id , dmsemis_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dmsemis_opt
  INTEGER id_id
  model_config_rec%dmsemis_opt = dmsemis_opt
  RETURN
END SUBROUTINE nl_set_dmsemis_opt
SUBROUTINE nl_set_seas_opt ( id_id , seas_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: seas_opt
  INTEGER id_id
  model_config_rec%seas_opt = seas_opt
  RETURN
END SUBROUTINE nl_set_seas_opt
SUBROUTINE nl_set_bio_emiss_opt ( id_id , bio_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bio_emiss_opt
  INTEGER id_id
  model_config_rec%bio_emiss_opt(id_id) = bio_emiss_opt
  RETURN
END SUBROUTINE nl_set_bio_emiss_opt
SUBROUTINE nl_set_biomass_burn_opt ( id_id , biomass_burn_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: biomass_burn_opt
  INTEGER id_id
  model_config_rec%biomass_burn_opt(id_id) = biomass_burn_opt
  RETURN
END SUBROUTINE nl_set_biomass_burn_opt
SUBROUTINE nl_set_plumerisefire_frq ( id_id , plumerisefire_frq )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: plumerisefire_frq
  INTEGER id_id
  model_config_rec%plumerisefire_frq(id_id) = plumerisefire_frq
  RETURN
END SUBROUTINE nl_set_plumerisefire_frq
SUBROUTINE nl_set_emiss_inpt_opt ( id_id , emiss_inpt_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: emiss_inpt_opt
  INTEGER id_id
  model_config_rec%emiss_inpt_opt(id_id) = emiss_inpt_opt
  RETURN
END SUBROUTINE nl_set_emiss_inpt_opt
SUBROUTINE nl_set_gas_bc_opt ( id_id , gas_bc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gas_bc_opt
  INTEGER id_id
  model_config_rec%gas_bc_opt(id_id) = gas_bc_opt
  RETURN
END SUBROUTINE nl_set_gas_bc_opt
SUBROUTINE nl_set_gas_ic_opt ( id_id , gas_ic_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gas_ic_opt
  INTEGER id_id
  model_config_rec%gas_ic_opt(id_id) = gas_ic_opt
  RETURN
END SUBROUTINE nl_set_gas_ic_opt
SUBROUTINE nl_set_aer_bc_opt ( id_id , aer_bc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_bc_opt
  INTEGER id_id
  model_config_rec%aer_bc_opt(id_id) = aer_bc_opt
  RETURN
END SUBROUTINE nl_set_aer_bc_opt
SUBROUTINE nl_set_aer_ic_opt ( id_id , aer_ic_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_ic_opt
  INTEGER id_id
  model_config_rec%aer_ic_opt(id_id) = aer_ic_opt
  RETURN
END SUBROUTINE nl_set_aer_ic_opt
SUBROUTINE nl_set_have_bcs_chem ( id_id , have_bcs_chem )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: have_bcs_chem
  INTEGER id_id
  model_config_rec%have_bcs_chem(id_id) = have_bcs_chem
  RETURN
END SUBROUTINE nl_set_have_bcs_chem
SUBROUTINE nl_set_have_bcs_tracer ( id_id , have_bcs_tracer )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: have_bcs_tracer
  INTEGER id_id
  model_config_rec%have_bcs_tracer(id_id) = have_bcs_tracer
  RETURN
END SUBROUTINE nl_set_have_bcs_tracer
SUBROUTINE nl_set_scale_fire_emiss ( id_id , scale_fire_emiss )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: scale_fire_emiss
  INTEGER id_id
  model_config_rec%scale_fire_emiss(id_id) = scale_fire_emiss
  RETURN
END SUBROUTINE nl_set_scale_fire_emiss
SUBROUTINE nl_set_aer_ra_feedback ( id_id , aer_ra_feedback )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_ra_feedback
  INTEGER id_id
  model_config_rec%aer_ra_feedback(id_id) = aer_ra_feedback
  RETURN
END SUBROUTINE nl_set_aer_ra_feedback
SUBROUTINE nl_set_aer_op_opt ( id_id , aer_op_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_op_opt
  INTEGER id_id
  model_config_rec%aer_op_opt(id_id) = aer_op_opt
  RETURN
END SUBROUTINE nl_set_aer_op_opt
SUBROUTINE nl_set_opt_pars_out ( id_id , opt_pars_out )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_pars_out
  INTEGER id_id
  model_config_rec%opt_pars_out = opt_pars_out
  RETURN
END SUBROUTINE nl_set_opt_pars_out
SUBROUTINE nl_set_diagnostic_dep ( id_id , diagnostic_dep )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: diagnostic_dep
  INTEGER id_id
  model_config_rec%diagnostic_dep(id_id) = diagnostic_dep
  RETURN
END SUBROUTINE nl_set_diagnostic_dep
SUBROUTINE nl_set_aircraft_emiss_opt ( id_id , aircraft_emiss_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aircraft_emiss_opt
  INTEGER id_id
  model_config_rec%aircraft_emiss_opt(id_id) = aircraft_emiss_opt
  RETURN
END SUBROUTINE nl_set_aircraft_emiss_opt
SUBROUTINE nl_set_have_bcs_upper ( id_id , have_bcs_upper )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: have_bcs_upper
  INTEGER id_id
  model_config_rec%have_bcs_upper(id_id) = have_bcs_upper
  RETURN
END SUBROUTINE nl_set_have_bcs_upper
SUBROUTINE nl_set_fixed_ubc_press ( id_id , fixed_ubc_press )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: fixed_ubc_press
  INTEGER id_id
  model_config_rec%fixed_ubc_press(id_id) = fixed_ubc_press
  RETURN
END SUBROUTINE nl_set_fixed_ubc_press
SUBROUTINE nl_set_fixed_ubc_inname ( id_id , fixed_ubc_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: fixed_ubc_inname
  INTEGER id_id
  model_config_rec%fixed_ubc_inname = trim(fixed_ubc_inname)
  RETURN
END SUBROUTINE nl_set_fixed_ubc_inname
SUBROUTINE nl_set_trop_lev_inname ( id_id , trop_lev_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: trop_lev_inname
  INTEGER id_id
  model_config_rec%trop_lev_inname = trim(trop_lev_inname)
  RETURN
END SUBROUTINE nl_set_trop_lev_inname
SUBROUTINE nl_set_exo_coldens_inname ( id_id , exo_coldens_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: exo_coldens_inname
  INTEGER id_id
  model_config_rec%exo_coldens_inname(id_id) = exo_coldens_inname
  RETURN
END SUBROUTINE nl_set_exo_coldens_inname
SUBROUTINE nl_set_wes_seasonal_inname ( id_id , wes_seasonal_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: wes_seasonal_inname
  INTEGER id_id
  model_config_rec%wes_seasonal_inname(id_id) = wes_seasonal_inname
  RETURN
END SUBROUTINE nl_set_wes_seasonal_inname
SUBROUTINE nl_set_chemdiag ( id_id , chemdiag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: chemdiag
  INTEGER id_id
  model_config_rec%chemdiag(id_id) = chemdiag
  RETURN
END SUBROUTINE nl_set_chemdiag
SUBROUTINE nl_set_aero_srf_area_diag ( id_id , aero_srf_area_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aero_srf_area_diag
  INTEGER id_id
  model_config_rec%aero_srf_area_diag(id_id) = aero_srf_area_diag
  RETURN
END SUBROUTINE nl_set_aero_srf_area_diag
SUBROUTINE nl_set_dust_alpha ( id_id , dust_alpha )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dust_alpha
  INTEGER id_id
  model_config_rec%dust_alpha = dust_alpha
  RETURN
END SUBROUTINE nl_set_dust_alpha
SUBROUTINE nl_set_dust_gamma ( id_id , dust_gamma )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dust_gamma
  INTEGER id_id
  model_config_rec%dust_gamma = dust_gamma
  RETURN
END SUBROUTINE nl_set_dust_gamma
SUBROUTINE nl_set_dust_smtune ( id_id , dust_smtune )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dust_smtune
  INTEGER id_id
  model_config_rec%dust_smtune = dust_smtune
  RETURN
END SUBROUTINE nl_set_dust_smtune
SUBROUTINE nl_set_dust_ustune ( id_id , dust_ustune )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dust_ustune
  INTEGER id_id
  model_config_rec%dust_ustune = dust_ustune
  RETURN
END SUBROUTINE nl_set_dust_ustune
SUBROUTINE nl_set_dust_dsr ( id_id , dust_dsr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_dsr
  INTEGER id_id
  model_config_rec%dust_dsr = dust_dsr
  RETURN
END SUBROUTINE nl_set_dust_dsr
SUBROUTINE nl_set_dust_veg ( id_id , dust_veg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_veg
  INTEGER id_id
  model_config_rec%dust_veg = dust_veg
  RETURN
END SUBROUTINE nl_set_dust_veg
SUBROUTINE nl_set_dust_soils ( id_id , dust_soils )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_soils
  INTEGER id_id
  model_config_rec%dust_soils = dust_soils
  RETURN
END SUBROUTINE nl_set_dust_soils
SUBROUTINE nl_set_dust_smois ( id_id , dust_smois )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_smois
  INTEGER id_id
  model_config_rec%dust_smois = dust_smois
  RETURN
END SUBROUTINE nl_set_dust_smois
SUBROUTINE nl_set_emiss_ash_hgt ( id_id , emiss_ash_hgt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: emiss_ash_hgt
  INTEGER id_id
  model_config_rec%emiss_ash_hgt = emiss_ash_hgt
  RETURN
END SUBROUTINE nl_set_emiss_ash_hgt
SUBROUTINE nl_set_depo_fact ( id_id , depo_fact )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: depo_fact
  INTEGER id_id
  model_config_rec%depo_fact(id_id) = depo_fact
  RETURN
END SUBROUTINE nl_set_depo_fact
SUBROUTINE nl_set_track_chem_num ( id_id , track_chem_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_chem_num
  INTEGER id_id
  model_config_rec%track_chem_num = track_chem_num
  RETURN
END SUBROUTINE nl_set_track_chem_num
SUBROUTINE nl_set_track_chem_name ( id_id , track_chem_name )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: track_chem_name
  INTEGER id_id
  model_config_rec%track_chem_name(id_id) = track_chem_name
  RETURN
END SUBROUTINE nl_set_track_chem_name
SUBROUTINE nl_set_track_rad_num ( id_id , track_rad_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_rad_num
  INTEGER id_id
  model_config_rec%track_rad_num = track_rad_num
  RETURN
END SUBROUTINE nl_set_track_rad_num
SUBROUTINE nl_set_track_tuv_num ( id_id , track_tuv_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_tuv_num
  INTEGER id_id
  model_config_rec%track_tuv_num = track_tuv_num
  RETURN
END SUBROUTINE nl_set_track_tuv_num
SUBROUTINE nl_set_track_tuv_lev ( id_id , track_tuv_lev )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_tuv_lev
  INTEGER id_id
  model_config_rec%track_tuv_lev = track_tuv_lev
  RETURN
END SUBROUTINE nl_set_track_tuv_lev
SUBROUTINE nl_set_n2o5_hetchem ( id_id , n2o5_hetchem )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: n2o5_hetchem
  INTEGER id_id
  model_config_rec%n2o5_hetchem = n2o5_hetchem
  RETURN
END SUBROUTINE nl_set_n2o5_hetchem
SUBROUTINE nl_set_mosaic_aerchem_optaa ( id_id , mosaic_aerchem_optaa )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mosaic_aerchem_optaa
  INTEGER id_id
  model_config_rec%mosaic_aerchem_optaa = mosaic_aerchem_optaa
  RETURN
END SUBROUTINE nl_set_mosaic_aerchem_optaa
SUBROUTINE nl_set_af_lambda_start ( id_id , af_lambda_start )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: af_lambda_start
  INTEGER id_id
  model_config_rec%af_lambda_start(id_id) = af_lambda_start
  RETURN
END SUBROUTINE nl_set_af_lambda_start
SUBROUTINE nl_set_af_lambda_end ( id_id , af_lambda_end )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: af_lambda_end
  INTEGER id_id
  model_config_rec%af_lambda_end(id_id) = af_lambda_end
  RETURN
END SUBROUTINE nl_set_af_lambda_end
SUBROUTINE nl_set_do_isorropia ( id_id , do_isorropia )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: do_isorropia
  INTEGER id_id
  model_config_rec%do_isorropia = do_isorropia
  RETURN
END SUBROUTINE nl_set_do_isorropia
SUBROUTINE nl_set_n_ic ( id_id , n_ic )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: n_ic
  INTEGER id_id
  model_config_rec%n_ic(id_id) = n_ic
  RETURN
END SUBROUTINE nl_set_n_ic
SUBROUTINE nl_set_n_cg ( id_id , n_cg )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: n_cg
  INTEGER id_id
  model_config_rec%n_cg(id_id) = n_cg
  RETURN
END SUBROUTINE nl_set_n_cg
SUBROUTINE nl_set_lnox_opt ( id_id , lnox_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: lnox_opt
  INTEGER id_id
  model_config_rec%lnox_opt(id_id) = lnox_opt
  RETURN
END SUBROUTINE nl_set_lnox_opt
SUBROUTINE nl_set_lnox_passive ( id_id , lnox_passive )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: lnox_passive
  INTEGER id_id
  model_config_rec%lnox_passive(id_id) = lnox_passive
  RETURN
END SUBROUTINE nl_set_lnox_passive
SUBROUTINE nl_set_ltng_temp_upper ( id_id , ltng_temp_upper )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ltng_temp_upper
  INTEGER id_id
  model_config_rec%ltng_temp_upper(id_id) = ltng_temp_upper
  RETURN
END SUBROUTINE nl_set_ltng_temp_upper
SUBROUTINE nl_set_ltng_temp_lower ( id_id , ltng_temp_lower )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ltng_temp_lower
  INTEGER id_id
  model_config_rec%ltng_temp_lower(id_id) = ltng_temp_lower
  RETURN
END SUBROUTINE nl_set_ltng_temp_lower
SUBROUTINE nl_set_has_o3_exo_coldens ( id_id , has_o3_exo_coldens )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: has_o3_exo_coldens
  INTEGER id_id
  model_config_rec%has_o3_exo_coldens = has_o3_exo_coldens
  RETURN
END SUBROUTINE nl_set_has_o3_exo_coldens
SUBROUTINE nl_set_du_at_grnd ( id_id , du_at_grnd )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: du_at_grnd
  INTEGER id_id
  model_config_rec%du_at_grnd = du_at_grnd
  RETURN
END SUBROUTINE nl_set_du_at_grnd
SUBROUTINE nl_set_scale_o3_to_grnd_exo_coldens ( id_id , scale_o3_to_grnd_exo_coldens )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: scale_o3_to_grnd_exo_coldens
  INTEGER id_id
  model_config_rec%scale_o3_to_grnd_exo_coldens = scale_o3_to_grnd_exo_coldens
  RETURN
END SUBROUTINE nl_set_scale_o3_to_grnd_exo_coldens
SUBROUTINE nl_set_scale_o3_to_du_at_grnd ( id_id , scale_o3_to_du_at_grnd )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: scale_o3_to_du_at_grnd
  INTEGER id_id
  model_config_rec%scale_o3_to_du_at_grnd = scale_o3_to_du_at_grnd
  RETURN
END SUBROUTINE nl_set_scale_o3_to_du_at_grnd
SUBROUTINE nl_set_irr_opt ( id_id , irr_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_opt
  INTEGER id_id
  model_config_rec%irr_opt(id_id) = irr_opt
  RETURN
END SUBROUTINE nl_set_irr_opt
SUBROUTINE nl_set_num_gca_levels ( id_id , num_gca_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_gca_levels
  INTEGER id_id
  model_config_rec%num_gca_levels = num_gca_levels
  RETURN
END SUBROUTINE nl_set_num_gca_levels
SUBROUTINE nl_set_gca_input_opt ( id_id , gca_input_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gca_input_opt
  INTEGER id_id
  model_config_rec%gca_input_opt = gca_input_opt
  RETURN
END SUBROUTINE nl_set_gca_input_opt
SUBROUTINE nl_set_run_days ( id_id , run_days )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: run_days
  INTEGER id_id
  model_config_rec%run_days = run_days
  RETURN
END SUBROUTINE nl_set_run_days
SUBROUTINE nl_set_run_hours ( id_id , run_hours )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: run_hours
  INTEGER id_id
  model_config_rec%run_hours = run_hours
  RETURN
END SUBROUTINE nl_set_run_hours
SUBROUTINE nl_set_run_minutes ( id_id , run_minutes )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: run_minutes
  INTEGER id_id
  model_config_rec%run_minutes = run_minutes
  RETURN
END SUBROUTINE nl_set_run_minutes
SUBROUTINE nl_set_run_seconds ( id_id , run_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: run_seconds
  INTEGER id_id
  model_config_rec%run_seconds = run_seconds
  RETURN
END SUBROUTINE nl_set_run_seconds
SUBROUTINE nl_set_start_year ( id_id , start_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_year
  INTEGER id_id
  model_config_rec%start_year(id_id) = start_year
  RETURN
END SUBROUTINE nl_set_start_year
SUBROUTINE nl_set_start_month ( id_id , start_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_month
  INTEGER id_id
  model_config_rec%start_month(id_id) = start_month
  RETURN
END SUBROUTINE nl_set_start_month
SUBROUTINE nl_set_start_day ( id_id , start_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_day
  INTEGER id_id
  model_config_rec%start_day(id_id) = start_day
  RETURN
END SUBROUTINE nl_set_start_day
SUBROUTINE nl_set_start_hour ( id_id , start_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_hour
  INTEGER id_id
  model_config_rec%start_hour(id_id) = start_hour
  RETURN
END SUBROUTINE nl_set_start_hour
SUBROUTINE nl_set_start_minute ( id_id , start_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_minute
  INTEGER id_id
  model_config_rec%start_minute(id_id) = start_minute
  RETURN
END SUBROUTINE nl_set_start_minute
SUBROUTINE nl_set_start_second ( id_id , start_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: start_second
  INTEGER id_id
  model_config_rec%start_second(id_id) = start_second
  RETURN
END SUBROUTINE nl_set_start_second
SUBROUTINE nl_set_end_year ( id_id , end_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_year
  INTEGER id_id
  model_config_rec%end_year(id_id) = end_year
  RETURN
END SUBROUTINE nl_set_end_year
SUBROUTINE nl_set_end_month ( id_id , end_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_month
  INTEGER id_id
  model_config_rec%end_month(id_id) = end_month
  RETURN
END SUBROUTINE nl_set_end_month
SUBROUTINE nl_set_end_day ( id_id , end_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_day
  INTEGER id_id
  model_config_rec%end_day(id_id) = end_day
  RETURN
END SUBROUTINE nl_set_end_day
SUBROUTINE nl_set_end_hour ( id_id , end_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_hour
  INTEGER id_id
  model_config_rec%end_hour(id_id) = end_hour
  RETURN
END SUBROUTINE nl_set_end_hour
SUBROUTINE nl_set_end_minute ( id_id , end_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_minute
  INTEGER id_id
  model_config_rec%end_minute(id_id) = end_minute
  RETURN
END SUBROUTINE nl_set_end_minute
SUBROUTINE nl_set_end_second ( id_id , end_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: end_second
  INTEGER id_id
  model_config_rec%end_second(id_id) = end_second
  RETURN
END SUBROUTINE nl_set_end_second
SUBROUTINE nl_set_interval_seconds ( id_id , interval_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: interval_seconds
  INTEGER id_id
  model_config_rec%interval_seconds = interval_seconds
  RETURN
END SUBROUTINE nl_set_interval_seconds
SUBROUTINE nl_set_input_from_file ( id_id , input_from_file )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: input_from_file
  INTEGER id_id
  model_config_rec%input_from_file(id_id) = input_from_file
  RETURN
END SUBROUTINE nl_set_input_from_file
SUBROUTINE nl_set_fine_input_stream ( id_id , fine_input_stream )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: fine_input_stream
  INTEGER id_id
  model_config_rec%fine_input_stream(id_id) = fine_input_stream
  RETURN
END SUBROUTINE nl_set_fine_input_stream
SUBROUTINE nl_set_input_from_hires ( id_id , input_from_hires )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: input_from_hires
  INTEGER id_id
  model_config_rec%input_from_hires(id_id) = input_from_hires
  RETURN
END SUBROUTINE nl_set_input_from_hires
SUBROUTINE nl_set_rsmas_data_path ( id_id , rsmas_data_path )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: rsmas_data_path
  INTEGER id_id
  model_config_rec%rsmas_data_path = trim(rsmas_data_path)
  RETURN
END SUBROUTINE nl_set_rsmas_data_path
SUBROUTINE nl_set_all_ic_times ( id_id , all_ic_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: all_ic_times
  INTEGER id_id
  model_config_rec%all_ic_times = all_ic_times
  RETURN
END SUBROUTINE nl_set_all_ic_times
SUBROUTINE nl_set_julyr ( id_id , julyr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: julyr
  INTEGER id_id
  model_config_rec%julyr(id_id) = julyr
  RETURN
END SUBROUTINE nl_set_julyr
SUBROUTINE nl_set_julday ( id_id , julday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: julday
  INTEGER id_id
  model_config_rec%julday(id_id) = julday
  RETURN
END SUBROUTINE nl_set_julday
SUBROUTINE nl_set_gmt ( id_id , gmt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: gmt
  INTEGER id_id
  model_config_rec%gmt(id_id) = gmt
  RETURN
END SUBROUTINE nl_set_gmt
SUBROUTINE nl_set_input_inname ( id_id , input_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: input_inname
  INTEGER id_id
  model_config_rec%input_inname = trim(input_inname)
  RETURN
END SUBROUTINE nl_set_input_inname
SUBROUTINE nl_set_input_outname ( id_id , input_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: input_outname
  INTEGER id_id
  model_config_rec%input_outname = trim(input_outname)
  RETURN
END SUBROUTINE nl_set_input_outname
SUBROUTINE nl_set_bdy_inname ( id_id , bdy_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: bdy_inname
  INTEGER id_id
  model_config_rec%bdy_inname = trim(bdy_inname)
  RETURN
END SUBROUTINE nl_set_bdy_inname
SUBROUTINE nl_set_bdy_outname ( id_id , bdy_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: bdy_outname
  INTEGER id_id
  model_config_rec%bdy_outname = trim(bdy_outname)
  RETURN
END SUBROUTINE nl_set_bdy_outname
SUBROUTINE nl_set_rst_inname ( id_id , rst_inname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: rst_inname
  INTEGER id_id
  model_config_rec%rst_inname = trim(rst_inname)
  RETURN
END SUBROUTINE nl_set_rst_inname
SUBROUTINE nl_set_rst_outname ( id_id , rst_outname )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: rst_outname
  INTEGER id_id
  model_config_rec%rst_outname = trim(rst_outname)
  RETURN
END SUBROUTINE nl_set_rst_outname
SUBROUTINE nl_set_write_input ( id_id , write_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: write_input
  INTEGER id_id
  model_config_rec%write_input = write_input
  RETURN
END SUBROUTINE nl_set_write_input
SUBROUTINE nl_set_write_restart_at_0h ( id_id , write_restart_at_0h )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: write_restart_at_0h
  INTEGER id_id
  model_config_rec%write_restart_at_0h = write_restart_at_0h
  RETURN
END SUBROUTINE nl_set_write_restart_at_0h
SUBROUTINE nl_set_write_hist_at_0h_rst ( id_id , write_hist_at_0h_rst )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: write_hist_at_0h_rst
  INTEGER id_id
  model_config_rec%write_hist_at_0h_rst = write_hist_at_0h_rst
  RETURN
END SUBROUTINE nl_set_write_hist_at_0h_rst
SUBROUTINE nl_set_adjust_output_times ( id_id , adjust_output_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: adjust_output_times
  INTEGER id_id
  model_config_rec%adjust_output_times = adjust_output_times
  RETURN
END SUBROUTINE nl_set_adjust_output_times
SUBROUTINE nl_set_adjust_input_times ( id_id , adjust_input_times )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: adjust_input_times
  INTEGER id_id
  model_config_rec%adjust_input_times = adjust_input_times
  RETURN
END SUBROUTINE nl_set_adjust_input_times
SUBROUTINE nl_set_diag_print ( id_id , diag_print )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: diag_print
  INTEGER id_id
  model_config_rec%diag_print = diag_print
  RETURN
END SUBROUTINE nl_set_diag_print
SUBROUTINE nl_set_nocolons ( id_id , nocolons )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: nocolons
  INTEGER id_id
  model_config_rec%nocolons = nocolons
  RETURN
END SUBROUTINE nl_set_nocolons
SUBROUTINE nl_set_cycling ( id_id , cycling )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: cycling
  INTEGER id_id
  model_config_rec%cycling = cycling
  RETURN
END SUBROUTINE nl_set_cycling
SUBROUTINE nl_set_output_diagnostics ( id_id , output_diagnostics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: output_diagnostics
  INTEGER id_id
  model_config_rec%output_diagnostics = output_diagnostics
  RETURN
END SUBROUTINE nl_set_output_diagnostics
SUBROUTINE nl_set_nwp_diagnostics ( id_id , nwp_diagnostics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nwp_diagnostics
  INTEGER id_id
  model_config_rec%nwp_diagnostics = nwp_diagnostics
  RETURN
END SUBROUTINE nl_set_nwp_diagnostics
SUBROUTINE nl_set_output_ready_flag ( id_id , output_ready_flag )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: output_ready_flag
  INTEGER id_id
  model_config_rec%output_ready_flag = output_ready_flag
  RETURN
END SUBROUTINE nl_set_output_ready_flag
SUBROUTINE nl_set_force_use_old_data ( id_id , force_use_old_data )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: force_use_old_data
  INTEGER id_id
  model_config_rec%force_use_old_data = force_use_old_data
  RETURN
END SUBROUTINE nl_set_force_use_old_data
SUBROUTINE nl_set_usepio ( id_id , usepio )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: usepio
  INTEGER id_id
  model_config_rec%usepio = usepio
  RETURN
END SUBROUTINE nl_set_usepio
SUBROUTINE nl_set_pioprocs ( id_id , pioprocs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: pioprocs
  INTEGER id_id
  model_config_rec%pioprocs = pioprocs
  RETURN
END SUBROUTINE nl_set_pioprocs
SUBROUTINE nl_set_piostart ( id_id , piostart )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: piostart
  INTEGER id_id
  model_config_rec%piostart = piostart
  RETURN
END SUBROUTINE nl_set_piostart
SUBROUTINE nl_set_piostride ( id_id , piostride )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: piostride
  INTEGER id_id
  model_config_rec%piostride = piostride
  RETURN
END SUBROUTINE nl_set_piostride
SUBROUTINE nl_set_pioshift ( id_id , pioshift )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: pioshift
  INTEGER id_id
  model_config_rec%pioshift = pioshift
  RETURN
END SUBROUTINE nl_set_pioshift
SUBROUTINE nl_set_dfi_opt ( id_id , dfi_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_opt
  INTEGER id_id
  model_config_rec%dfi_opt = dfi_opt
  RETURN
END SUBROUTINE nl_set_dfi_opt
SUBROUTINE nl_set_dfi_savehydmeteors ( id_id , dfi_savehydmeteors )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_savehydmeteors
  INTEGER id_id
  model_config_rec%dfi_savehydmeteors = dfi_savehydmeteors
  RETURN
END SUBROUTINE nl_set_dfi_savehydmeteors
SUBROUTINE nl_set_dfi_nfilter ( id_id , dfi_nfilter )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_nfilter
  INTEGER id_id
  model_config_rec%dfi_nfilter = dfi_nfilter
  RETURN
END SUBROUTINE nl_set_dfi_nfilter
SUBROUTINE nl_set_dfi_write_filtered_input ( id_id , dfi_write_filtered_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: dfi_write_filtered_input
  INTEGER id_id
  model_config_rec%dfi_write_filtered_input = dfi_write_filtered_input
  RETURN
END SUBROUTINE nl_set_dfi_write_filtered_input
SUBROUTINE nl_set_dfi_write_dfi_history ( id_id , dfi_write_dfi_history )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: dfi_write_dfi_history
  INTEGER id_id
  model_config_rec%dfi_write_dfi_history = dfi_write_dfi_history
  RETURN
END SUBROUTINE nl_set_dfi_write_dfi_history
SUBROUTINE nl_set_dfi_cutoff_seconds ( id_id , dfi_cutoff_seconds )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_cutoff_seconds
  INTEGER id_id
  model_config_rec%dfi_cutoff_seconds = dfi_cutoff_seconds
  RETURN
END SUBROUTINE nl_set_dfi_cutoff_seconds
SUBROUTINE nl_set_dfi_time_dim ( id_id , dfi_time_dim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_time_dim
  INTEGER id_id
  model_config_rec%dfi_time_dim = dfi_time_dim
  RETURN
END SUBROUTINE nl_set_dfi_time_dim
SUBROUTINE nl_set_dfi_fwdstop_year ( id_id , dfi_fwdstop_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_year
  INTEGER id_id
  model_config_rec%dfi_fwdstop_year = dfi_fwdstop_year
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_year
SUBROUTINE nl_set_dfi_fwdstop_month ( id_id , dfi_fwdstop_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_month
  INTEGER id_id
  model_config_rec%dfi_fwdstop_month = dfi_fwdstop_month
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_month
SUBROUTINE nl_set_dfi_fwdstop_day ( id_id , dfi_fwdstop_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_day
  INTEGER id_id
  model_config_rec%dfi_fwdstop_day = dfi_fwdstop_day
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_day
SUBROUTINE nl_set_dfi_fwdstop_hour ( id_id , dfi_fwdstop_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_hour
  INTEGER id_id
  model_config_rec%dfi_fwdstop_hour = dfi_fwdstop_hour
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_hour
SUBROUTINE nl_set_dfi_fwdstop_minute ( id_id , dfi_fwdstop_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_minute
  INTEGER id_id
  model_config_rec%dfi_fwdstop_minute = dfi_fwdstop_minute
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_minute
SUBROUTINE nl_set_dfi_fwdstop_second ( id_id , dfi_fwdstop_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_fwdstop_second
  INTEGER id_id
  model_config_rec%dfi_fwdstop_second = dfi_fwdstop_second
  RETURN
END SUBROUTINE nl_set_dfi_fwdstop_second
SUBROUTINE nl_set_dfi_bckstop_year ( id_id , dfi_bckstop_year )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_year
  INTEGER id_id
  model_config_rec%dfi_bckstop_year = dfi_bckstop_year
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_year
SUBROUTINE nl_set_dfi_bckstop_month ( id_id , dfi_bckstop_month )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_month
  INTEGER id_id
  model_config_rec%dfi_bckstop_month = dfi_bckstop_month
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_month
SUBROUTINE nl_set_dfi_bckstop_day ( id_id , dfi_bckstop_day )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_day
  INTEGER id_id
  model_config_rec%dfi_bckstop_day = dfi_bckstop_day
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_day
SUBROUTINE nl_set_dfi_bckstop_hour ( id_id , dfi_bckstop_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_hour
  INTEGER id_id
  model_config_rec%dfi_bckstop_hour = dfi_bckstop_hour
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_hour
SUBROUTINE nl_set_dfi_bckstop_minute ( id_id , dfi_bckstop_minute )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_minute
  INTEGER id_id
  model_config_rec%dfi_bckstop_minute = dfi_bckstop_minute
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_minute
SUBROUTINE nl_set_dfi_bckstop_second ( id_id , dfi_bckstop_second )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dfi_bckstop_second
  INTEGER id_id
  model_config_rec%dfi_bckstop_second = dfi_bckstop_second
  RETURN
END SUBROUTINE nl_set_dfi_bckstop_second
SUBROUTINE nl_set_time_step ( id_id , time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: time_step
  INTEGER id_id
  model_config_rec%time_step = time_step
  RETURN
END SUBROUTINE nl_set_time_step
SUBROUTINE nl_set_time_step_fract_num ( id_id , time_step_fract_num )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: time_step_fract_num
  INTEGER id_id
  model_config_rec%time_step_fract_num = time_step_fract_num
  RETURN
END SUBROUTINE nl_set_time_step_fract_num
SUBROUTINE nl_set_time_step_fract_den ( id_id , time_step_fract_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: time_step_fract_den
  INTEGER id_id
  model_config_rec%time_step_fract_den = time_step_fract_den
  RETURN
END SUBROUTINE nl_set_time_step_fract_den
SUBROUTINE nl_set_time_step_dfi ( id_id , time_step_dfi )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: time_step_dfi
  INTEGER id_id
  model_config_rec%time_step_dfi = time_step_dfi
  RETURN
END SUBROUTINE nl_set_time_step_dfi
SUBROUTINE nl_set_reasonable_time_step_ratio ( id_id , reasonable_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: reasonable_time_step_ratio
  INTEGER id_id
  model_config_rec%reasonable_time_step_ratio = reasonable_time_step_ratio
  RETURN
END SUBROUTINE nl_set_reasonable_time_step_ratio
SUBROUTINE nl_set_min_time_step ( id_id , min_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: min_time_step
  INTEGER id_id
  model_config_rec%min_time_step(id_id) = min_time_step
  RETURN
END SUBROUTINE nl_set_min_time_step
SUBROUTINE nl_set_min_time_step_den ( id_id , min_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: min_time_step_den
  INTEGER id_id
  model_config_rec%min_time_step_den(id_id) = min_time_step_den
  RETURN
END SUBROUTINE nl_set_min_time_step_den
SUBROUTINE nl_set_max_time_step ( id_id , max_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_time_step
  INTEGER id_id
  model_config_rec%max_time_step(id_id) = max_time_step
  RETURN
END SUBROUTINE nl_set_max_time_step
SUBROUTINE nl_set_max_time_step_den ( id_id , max_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_time_step_den
  INTEGER id_id
  model_config_rec%max_time_step_den(id_id) = max_time_step_den
  RETURN
END SUBROUTINE nl_set_max_time_step_den
SUBROUTINE nl_set_target_cfl ( id_id , target_cfl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: target_cfl
  INTEGER id_id
  model_config_rec%target_cfl(id_id) = target_cfl
  RETURN
END SUBROUTINE nl_set_target_cfl
SUBROUTINE nl_set_target_hcfl ( id_id , target_hcfl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: target_hcfl
  INTEGER id_id
  model_config_rec%target_hcfl(id_id) = target_hcfl
  RETURN
END SUBROUTINE nl_set_target_hcfl
SUBROUTINE nl_set_max_step_increase_pct ( id_id , max_step_increase_pct )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_step_increase_pct
  INTEGER id_id
  model_config_rec%max_step_increase_pct(id_id) = max_step_increase_pct
  RETURN
END SUBROUTINE nl_set_max_step_increase_pct
SUBROUTINE nl_set_starting_time_step ( id_id , starting_time_step )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: starting_time_step
  INTEGER id_id
  model_config_rec%starting_time_step(id_id) = starting_time_step
  RETURN
END SUBROUTINE nl_set_starting_time_step
SUBROUTINE nl_set_starting_time_step_den ( id_id , starting_time_step_den )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: starting_time_step_den
  INTEGER id_id
  model_config_rec%starting_time_step_den(id_id) = starting_time_step_den
  RETURN
END SUBROUTINE nl_set_starting_time_step_den
SUBROUTINE nl_set_step_to_output_time ( id_id , step_to_output_time )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: step_to_output_time
  INTEGER id_id
  model_config_rec%step_to_output_time = step_to_output_time
  RETURN
END SUBROUTINE nl_set_step_to_output_time
SUBROUTINE nl_set_adaptation_domain ( id_id , adaptation_domain )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: adaptation_domain
  INTEGER id_id
  model_config_rec%adaptation_domain = adaptation_domain
  RETURN
END SUBROUTINE nl_set_adaptation_domain
SUBROUTINE nl_set_use_adaptive_time_step ( id_id , use_adaptive_time_step )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_adaptive_time_step
  INTEGER id_id
  model_config_rec%use_adaptive_time_step = use_adaptive_time_step
  RETURN
END SUBROUTINE nl_set_use_adaptive_time_step
SUBROUTINE nl_set_use_adaptive_time_step_dfi ( id_id , use_adaptive_time_step_dfi )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_adaptive_time_step_dfi
  INTEGER id_id
  model_config_rec%use_adaptive_time_step_dfi = use_adaptive_time_step_dfi
  RETURN
END SUBROUTINE nl_set_use_adaptive_time_step_dfi
SUBROUTINE nl_set_max_dom ( id_id , max_dom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_dom
  INTEGER id_id
  model_config_rec%max_dom = max_dom
  RETURN
END SUBROUTINE nl_set_max_dom
SUBROUTINE nl_set_lats_to_mic ( id_id , lats_to_mic )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: lats_to_mic
  INTEGER id_id
  model_config_rec%lats_to_mic = lats_to_mic
  RETURN
END SUBROUTINE nl_set_lats_to_mic
SUBROUTINE nl_set_s_we ( id_id , s_we )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: s_we
  INTEGER id_id
  model_config_rec%s_we(id_id) = s_we
  RETURN
END SUBROUTINE nl_set_s_we
SUBROUTINE nl_set_e_we ( id_id , e_we )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: e_we
  INTEGER id_id
  model_config_rec%e_we(id_id) = e_we
  RETURN
END SUBROUTINE nl_set_e_we
SUBROUTINE nl_set_s_sn ( id_id , s_sn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: s_sn
  INTEGER id_id
  model_config_rec%s_sn(id_id) = s_sn
  RETURN
END SUBROUTINE nl_set_s_sn
SUBROUTINE nl_set_e_sn ( id_id , e_sn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: e_sn
  INTEGER id_id
  model_config_rec%e_sn(id_id) = e_sn
  RETURN
END SUBROUTINE nl_set_e_sn
SUBROUTINE nl_set_s_vert ( id_id , s_vert )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: s_vert
  INTEGER id_id
  model_config_rec%s_vert(id_id) = s_vert
  RETURN
END SUBROUTINE nl_set_s_vert
SUBROUTINE nl_set_e_vert ( id_id , e_vert )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: e_vert
  INTEGER id_id
  model_config_rec%e_vert(id_id) = e_vert
  RETURN
END SUBROUTINE nl_set_e_vert
SUBROUTINE nl_set_num_metgrid_levels ( id_id , num_metgrid_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_metgrid_levels
  INTEGER id_id
  model_config_rec%num_metgrid_levels = num_metgrid_levels
  RETURN
END SUBROUTINE nl_set_num_metgrid_levels
SUBROUTINE nl_set_num_metgrid_soil_levels ( id_id , num_metgrid_soil_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_metgrid_soil_levels
  INTEGER id_id
  model_config_rec%num_metgrid_soil_levels = num_metgrid_soil_levels
  RETURN
END SUBROUTINE nl_set_num_metgrid_soil_levels
SUBROUTINE nl_set_p_top_requested ( id_id , p_top_requested )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: p_top_requested
  INTEGER id_id
  model_config_rec%p_top_requested = p_top_requested
  RETURN
END SUBROUTINE nl_set_p_top_requested
SUBROUTINE nl_set_interp_theta ( id_id , interp_theta )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: interp_theta
  INTEGER id_id
  model_config_rec%interp_theta = interp_theta
  RETURN
END SUBROUTINE nl_set_interp_theta
SUBROUTINE nl_set_interp_type ( id_id , interp_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: interp_type
  INTEGER id_id
  model_config_rec%interp_type = interp_type
  RETURN
END SUBROUTINE nl_set_interp_type
SUBROUTINE nl_set_rebalance ( id_id , rebalance )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: rebalance
  INTEGER id_id
  model_config_rec%rebalance = rebalance
  RETURN
END SUBROUTINE nl_set_rebalance
SUBROUTINE nl_set_vert_refine_method ( id_id , vert_refine_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: vert_refine_method
  INTEGER id_id
  model_config_rec%vert_refine_method(id_id) = vert_refine_method
  RETURN
END SUBROUTINE nl_set_vert_refine_method
SUBROUTINE nl_set_vert_refine_fact ( id_id , vert_refine_fact )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: vert_refine_fact
  INTEGER id_id
  model_config_rec%vert_refine_fact = vert_refine_fact
  RETURN
END SUBROUTINE nl_set_vert_refine_fact
SUBROUTINE nl_set_extrap_type ( id_id , extrap_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: extrap_type
  INTEGER id_id
  model_config_rec%extrap_type = extrap_type
  RETURN
END SUBROUTINE nl_set_extrap_type
SUBROUTINE nl_set_t_extrap_type ( id_id , t_extrap_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: t_extrap_type
  INTEGER id_id
  model_config_rec%t_extrap_type = t_extrap_type
  RETURN
END SUBROUTINE nl_set_t_extrap_type
SUBROUTINE nl_set_hypsometric_opt ( id_id , hypsometric_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: hypsometric_opt
  INTEGER id_id
  model_config_rec%hypsometric_opt = hypsometric_opt
  RETURN
END SUBROUTINE nl_set_hypsometric_opt
SUBROUTINE nl_set_lowest_lev_from_sfc ( id_id , lowest_lev_from_sfc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: lowest_lev_from_sfc
  INTEGER id_id
  model_config_rec%lowest_lev_from_sfc = lowest_lev_from_sfc
  RETURN
END SUBROUTINE nl_set_lowest_lev_from_sfc
SUBROUTINE nl_set_use_levels_below_ground ( id_id , use_levels_below_ground )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_levels_below_ground
  INTEGER id_id
  model_config_rec%use_levels_below_ground = use_levels_below_ground
  RETURN
END SUBROUTINE nl_set_use_levels_below_ground
SUBROUTINE nl_set_use_tavg_for_tsk ( id_id , use_tavg_for_tsk )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_tavg_for_tsk
  INTEGER id_id
  model_config_rec%use_tavg_for_tsk = use_tavg_for_tsk
  RETURN
END SUBROUTINE nl_set_use_tavg_for_tsk
SUBROUTINE nl_set_use_surface ( id_id , use_surface )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_surface
  INTEGER id_id
  model_config_rec%use_surface = use_surface
  RETURN
END SUBROUTINE nl_set_use_surface
SUBROUTINE nl_set_lagrange_order ( id_id , lagrange_order )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: lagrange_order
  INTEGER id_id
  model_config_rec%lagrange_order = lagrange_order
  RETURN
END SUBROUTINE nl_set_lagrange_order
SUBROUTINE nl_set_linear_interp ( id_id , linear_interp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: linear_interp
  INTEGER id_id
  model_config_rec%linear_interp = linear_interp
  RETURN
END SUBROUTINE nl_set_linear_interp
SUBROUTINE nl_set_force_sfc_in_vinterp ( id_id , force_sfc_in_vinterp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: force_sfc_in_vinterp
  INTEGER id_id
  model_config_rec%force_sfc_in_vinterp = force_sfc_in_vinterp
  RETURN
END SUBROUTINE nl_set_force_sfc_in_vinterp
SUBROUTINE nl_set_zap_close_levels ( id_id , zap_close_levels )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: zap_close_levels
  INTEGER id_id
  model_config_rec%zap_close_levels = zap_close_levels
  RETURN
END SUBROUTINE nl_set_zap_close_levels
SUBROUTINE nl_set_maxw_horiz_pres_diff ( id_id , maxw_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: maxw_horiz_pres_diff
  INTEGER id_id
  model_config_rec%maxw_horiz_pres_diff = maxw_horiz_pres_diff
  RETURN
END SUBROUTINE nl_set_maxw_horiz_pres_diff
SUBROUTINE nl_set_trop_horiz_pres_diff ( id_id , trop_horiz_pres_diff )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: trop_horiz_pres_diff
  INTEGER id_id
  model_config_rec%trop_horiz_pres_diff = trop_horiz_pres_diff
  RETURN
END SUBROUTINE nl_set_trop_horiz_pres_diff
SUBROUTINE nl_set_maxw_above_this_level ( id_id , maxw_above_this_level )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: maxw_above_this_level
  INTEGER id_id
  model_config_rec%maxw_above_this_level = maxw_above_this_level
  RETURN
END SUBROUTINE nl_set_maxw_above_this_level
SUBROUTINE nl_set_use_maxw_level ( id_id , use_maxw_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: use_maxw_level
  INTEGER id_id
  model_config_rec%use_maxw_level = use_maxw_level
  RETURN
END SUBROUTINE nl_set_use_maxw_level
SUBROUTINE nl_set_use_trop_level ( id_id , use_trop_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: use_trop_level
  INTEGER id_id
  model_config_rec%use_trop_level = use_trop_level
  RETURN
END SUBROUTINE nl_set_use_trop_level
SUBROUTINE nl_set_sfcp_to_sfcp ( id_id , sfcp_to_sfcp )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: sfcp_to_sfcp
  INTEGER id_id
  model_config_rec%sfcp_to_sfcp = sfcp_to_sfcp
  RETURN
END SUBROUTINE nl_set_sfcp_to_sfcp
SUBROUTINE nl_set_adjust_heights ( id_id , adjust_heights )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: adjust_heights
  INTEGER id_id
  model_config_rec%adjust_heights = adjust_heights
  RETURN
END SUBROUTINE nl_set_adjust_heights
SUBROUTINE nl_set_smooth_cg_topo ( id_id , smooth_cg_topo )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: smooth_cg_topo
  INTEGER id_id
  model_config_rec%smooth_cg_topo = smooth_cg_topo
  RETURN
END SUBROUTINE nl_set_smooth_cg_topo
SUBROUTINE nl_set_nest_interp_coord ( id_id , nest_interp_coord )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nest_interp_coord
  INTEGER id_id
  model_config_rec%nest_interp_coord = nest_interp_coord
  RETURN
END SUBROUTINE nl_set_nest_interp_coord
SUBROUTINE nl_set_interp_method_type ( id_id , interp_method_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: interp_method_type
  INTEGER id_id
  model_config_rec%interp_method_type = interp_method_type
  RETURN
END SUBROUTINE nl_set_interp_method_type
SUBROUTINE nl_set_aggregate_lu ( id_id , aggregate_lu )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: aggregate_lu
  INTEGER id_id
  model_config_rec%aggregate_lu = aggregate_lu
  RETURN
END SUBROUTINE nl_set_aggregate_lu
SUBROUTINE nl_set_rh2qv_wrt_liquid ( id_id , rh2qv_wrt_liquid )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: rh2qv_wrt_liquid
  INTEGER id_id
  model_config_rec%rh2qv_wrt_liquid = rh2qv_wrt_liquid
  RETURN
END SUBROUTINE nl_set_rh2qv_wrt_liquid
SUBROUTINE nl_set_rh2qv_method ( id_id , rh2qv_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: rh2qv_method
  INTEGER id_id
  model_config_rec%rh2qv_method = rh2qv_method
  RETURN
END SUBROUTINE nl_set_rh2qv_method
SUBROUTINE nl_set_qv_max_p_safe ( id_id , qv_max_p_safe )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_max_p_safe
  INTEGER id_id
  model_config_rec%qv_max_p_safe = qv_max_p_safe
  RETURN
END SUBROUTINE nl_set_qv_max_p_safe
SUBROUTINE nl_set_qv_max_flag ( id_id , qv_max_flag )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_max_flag
  INTEGER id_id
  model_config_rec%qv_max_flag = qv_max_flag
  RETURN
END SUBROUTINE nl_set_qv_max_flag
SUBROUTINE nl_set_qv_max_value ( id_id , qv_max_value )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_max_value
  INTEGER id_id
  model_config_rec%qv_max_value = qv_max_value
  RETURN
END SUBROUTINE nl_set_qv_max_value
SUBROUTINE nl_set_qv_min_p_safe ( id_id , qv_min_p_safe )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_min_p_safe
  INTEGER id_id
  model_config_rec%qv_min_p_safe = qv_min_p_safe
  RETURN
END SUBROUTINE nl_set_qv_min_p_safe
SUBROUTINE nl_set_qv_min_flag ( id_id , qv_min_flag )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_min_flag
  INTEGER id_id
  model_config_rec%qv_min_flag = qv_min_flag
  RETURN
END SUBROUTINE nl_set_qv_min_flag
SUBROUTINE nl_set_qv_min_value ( id_id , qv_min_value )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: qv_min_value
  INTEGER id_id
  model_config_rec%qv_min_value = qv_min_value
  RETURN
END SUBROUTINE nl_set_qv_min_value
SUBROUTINE nl_set_ideal_init_method ( id_id , ideal_init_method )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ideal_init_method
  INTEGER id_id
  model_config_rec%ideal_init_method = ideal_init_method
  RETURN
END SUBROUTINE nl_set_ideal_init_method
SUBROUTINE nl_set_dx ( id_id , dx )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dx
  INTEGER id_id
  model_config_rec%dx(id_id) = dx
  RETURN
END SUBROUTINE nl_set_dx
SUBROUTINE nl_set_dy ( id_id , dy )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dy
  INTEGER id_id
  model_config_rec%dy(id_id) = dy
  RETURN
END SUBROUTINE nl_set_dy
SUBROUTINE nl_set_grid_id ( id_id , grid_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: grid_id
  INTEGER id_id
  model_config_rec%grid_id(id_id) = grid_id
  RETURN
END SUBROUTINE nl_set_grid_id
SUBROUTINE nl_set_grid_allowed ( id_id , grid_allowed )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: grid_allowed
  INTEGER id_id
  model_config_rec%grid_allowed(id_id) = grid_allowed
  RETURN
END SUBROUTINE nl_set_grid_allowed
SUBROUTINE nl_set_parent_id ( id_id , parent_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: parent_id
  INTEGER id_id
  model_config_rec%parent_id(id_id) = parent_id
  RETURN
END SUBROUTINE nl_set_parent_id
SUBROUTINE nl_set_i_parent_start ( id_id , i_parent_start )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: i_parent_start
  INTEGER id_id
  model_config_rec%i_parent_start(id_id) = i_parent_start
  RETURN
END SUBROUTINE nl_set_i_parent_start
SUBROUTINE nl_set_j_parent_start ( id_id , j_parent_start )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: j_parent_start
  INTEGER id_id
  model_config_rec%j_parent_start(id_id) = j_parent_start
  RETURN
END SUBROUTINE nl_set_j_parent_start
SUBROUTINE nl_set_parent_grid_ratio ( id_id , parent_grid_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: parent_grid_ratio
  INTEGER id_id
  model_config_rec%parent_grid_ratio(id_id) = parent_grid_ratio
  RETURN
END SUBROUTINE nl_set_parent_grid_ratio
SUBROUTINE nl_set_parent_time_step_ratio ( id_id , parent_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: parent_time_step_ratio
  INTEGER id_id
  model_config_rec%parent_time_step_ratio(id_id) = parent_time_step_ratio
  RETURN
END SUBROUTINE nl_set_parent_time_step_ratio
SUBROUTINE nl_set_feedback ( id_id , feedback )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: feedback
  INTEGER id_id
  model_config_rec%feedback = feedback
  RETURN
END SUBROUTINE nl_set_feedback
SUBROUTINE nl_set_smooth_option ( id_id , smooth_option )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: smooth_option
  INTEGER id_id
  model_config_rec%smooth_option = smooth_option
  RETURN
END SUBROUTINE nl_set_smooth_option
SUBROUTINE nl_set_blend_width ( id_id , blend_width )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: blend_width
  INTEGER id_id
  model_config_rec%blend_width = blend_width
  RETURN
END SUBROUTINE nl_set_blend_width
SUBROUTINE nl_set_ztop ( id_id , ztop )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ztop
  INTEGER id_id
  model_config_rec%ztop(id_id) = ztop
  RETURN
END SUBROUTINE nl_set_ztop
SUBROUTINE nl_set_moad_grid_ratio ( id_id , moad_grid_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: moad_grid_ratio
  INTEGER id_id
  model_config_rec%moad_grid_ratio(id_id) = moad_grid_ratio
  RETURN
END SUBROUTINE nl_set_moad_grid_ratio
SUBROUTINE nl_set_moad_time_step_ratio ( id_id , moad_time_step_ratio )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: moad_time_step_ratio
  INTEGER id_id
  model_config_rec%moad_time_step_ratio(id_id) = moad_time_step_ratio
  RETURN
END SUBROUTINE nl_set_moad_time_step_ratio
SUBROUTINE nl_set_shw ( id_id , shw )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shw
  INTEGER id_id
  model_config_rec%shw(id_id) = shw
  RETURN
END SUBROUTINE nl_set_shw
SUBROUTINE nl_set_tile_sz_x ( id_id , tile_sz_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tile_sz_x
  INTEGER id_id
  model_config_rec%tile_sz_x = tile_sz_x
  RETURN
END SUBROUTINE nl_set_tile_sz_x
SUBROUTINE nl_set_tile_sz_y ( id_id , tile_sz_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tile_sz_y
  INTEGER id_id
  model_config_rec%tile_sz_y = tile_sz_y
  RETURN
END SUBROUTINE nl_set_tile_sz_y
SUBROUTINE nl_set_numtiles ( id_id , numtiles )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: numtiles
  INTEGER id_id
  model_config_rec%numtiles = numtiles
  RETURN
END SUBROUTINE nl_set_numtiles
SUBROUTINE nl_set_numtiles_inc ( id_id , numtiles_inc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: numtiles_inc
  INTEGER id_id
  model_config_rec%numtiles_inc = numtiles_inc
  RETURN
END SUBROUTINE nl_set_numtiles_inc
SUBROUTINE nl_set_numtiles_x ( id_id , numtiles_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: numtiles_x
  INTEGER id_id
  model_config_rec%numtiles_x = numtiles_x
  RETURN
END SUBROUTINE nl_set_numtiles_x
SUBROUTINE nl_set_numtiles_y ( id_id , numtiles_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: numtiles_y
  INTEGER id_id
  model_config_rec%numtiles_y = numtiles_y
  RETURN
END SUBROUTINE nl_set_numtiles_y
SUBROUTINE nl_set_tile_strategy ( id_id , tile_strategy )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tile_strategy
  INTEGER id_id
  model_config_rec%tile_strategy = tile_strategy
  RETURN
END SUBROUTINE nl_set_tile_strategy
SUBROUTINE nl_set_nproc_x ( id_id , nproc_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nproc_x
  INTEGER id_id
  model_config_rec%nproc_x = nproc_x
  RETURN
END SUBROUTINE nl_set_nproc_x
SUBROUTINE nl_set_nproc_y ( id_id , nproc_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nproc_y
  INTEGER id_id
  model_config_rec%nproc_y = nproc_y
  RETURN
END SUBROUTINE nl_set_nproc_y
SUBROUTINE nl_set_irand ( id_id , irand )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irand
  INTEGER id_id
  model_config_rec%irand = irand
  RETURN
END SUBROUTINE nl_set_irand
SUBROUTINE nl_set_dt ( id_id , dt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dt
  INTEGER id_id
  model_config_rec%dt(id_id) = dt
  RETURN
END SUBROUTINE nl_set_dt
SUBROUTINE nl_set_fft_used ( id_id , fft_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: fft_used
  INTEGER id_id
  model_config_rec%fft_used = fft_used
  RETURN
END SUBROUTINE nl_set_fft_used
SUBROUTINE nl_set_cu_used ( id_id , cu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cu_used
  INTEGER id_id
  model_config_rec%cu_used = cu_used
  RETURN
END SUBROUTINE nl_set_cu_used
SUBROUTINE nl_set_shcu_used ( id_id , shcu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shcu_used
  INTEGER id_id
  model_config_rec%shcu_used = shcu_used
  RETURN
END SUBROUTINE nl_set_shcu_used
SUBROUTINE nl_set_cam_used ( id_id , cam_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cam_used
  INTEGER id_id
  model_config_rec%cam_used = cam_used
  RETURN
END SUBROUTINE nl_set_cam_used
SUBROUTINE nl_set_gwd_used ( id_id , gwd_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gwd_used
  INTEGER id_id
  model_config_rec%gwd_used = gwd_used
  RETURN
END SUBROUTINE nl_set_gwd_used
SUBROUTINE nl_set_gwd_diags_used ( id_id , gwd_diags_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gwd_diags_used
  INTEGER id_id
  model_config_rec%gwd_diags_used = gwd_diags_used
  RETURN
END SUBROUTINE nl_set_gwd_diags_used
SUBROUTINE nl_set_alloc_qndropsource ( id_id , alloc_qndropsource )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: alloc_qndropsource
  INTEGER id_id
  model_config_rec%alloc_qndropsource = alloc_qndropsource
  RETURN
END SUBROUTINE nl_set_alloc_qndropsource
SUBROUTINE nl_set_num_moves ( id_id , num_moves )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_moves
  INTEGER id_id
  model_config_rec%num_moves = num_moves
  RETURN
END SUBROUTINE nl_set_num_moves
SUBROUTINE nl_set_ts_buf_size ( id_id , ts_buf_size )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ts_buf_size
  INTEGER id_id
  model_config_rec%ts_buf_size = ts_buf_size
  RETURN
END SUBROUTINE nl_set_ts_buf_size
SUBROUTINE nl_set_max_ts_locs ( id_id , max_ts_locs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_ts_locs
  INTEGER id_id
  model_config_rec%max_ts_locs = max_ts_locs
  RETURN
END SUBROUTINE nl_set_max_ts_locs
SUBROUTINE nl_set_tslist_ij ( id_id , tslist_ij )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: tslist_ij
  INTEGER id_id
  model_config_rec%tslist_ij = tslist_ij
  RETURN
END SUBROUTINE nl_set_tslist_ij
SUBROUTINE nl_set_tslist_unstagger_winds ( id_id , tslist_unstagger_winds )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: tslist_unstagger_winds
  INTEGER id_id
  model_config_rec%tslist_unstagger_winds = tslist_unstagger_winds
  RETURN
END SUBROUTINE nl_set_tslist_unstagger_winds
SUBROUTINE nl_set_vortex_interval ( id_id , vortex_interval )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: vortex_interval
  INTEGER id_id
  model_config_rec%vortex_interval(id_id) = vortex_interval
  RETURN
END SUBROUTINE nl_set_vortex_interval
!ENDOFREGISTRYGENERATEDINCLUDE
