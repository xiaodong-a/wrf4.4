!STARTOFREGISTRYGENERATEDINCLUDE 'inc/nl_config.inc'
!
! WARNING This file is generated automatically by use_registry
! using the data base in the file named Registry.
! Do not edit. Your changes to this file will be lost.
!
SUBROUTINE nl_set_max_vortex_speed ( id_id , max_vortex_speed )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_vortex_speed
  INTEGER id_id
  model_config_rec%max_vortex_speed(id_id) = max_vortex_speed
  RETURN
END SUBROUTINE nl_set_max_vortex_speed
SUBROUTINE nl_set_corral_dist ( id_id , corral_dist )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: corral_dist
  INTEGER id_id
  model_config_rec%corral_dist(id_id) = corral_dist
  RETURN
END SUBROUTINE nl_set_corral_dist
SUBROUTINE nl_set_track_level ( id_id , track_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_level
  INTEGER id_id
  model_config_rec%track_level = track_level
  RETURN
END SUBROUTINE nl_set_track_level
SUBROUTINE nl_set_time_to_move ( id_id , time_to_move )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: time_to_move
  INTEGER id_id
  model_config_rec%time_to_move(id_id) = time_to_move
  RETURN
END SUBROUTINE nl_set_time_to_move
SUBROUTINE nl_set_move_id ( id_id , move_id )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: move_id
  INTEGER id_id
  model_config_rec%move_id(id_id) = move_id
  RETURN
END SUBROUTINE nl_set_move_id
SUBROUTINE nl_set_move_interval ( id_id , move_interval )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: move_interval
  INTEGER id_id
  model_config_rec%move_interval(id_id) = move_interval
  RETURN
END SUBROUTINE nl_set_move_interval
SUBROUTINE nl_set_move_cd_x ( id_id , move_cd_x )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: move_cd_x
  INTEGER id_id
  model_config_rec%move_cd_x(id_id) = move_cd_x
  RETURN
END SUBROUTINE nl_set_move_cd_x
SUBROUTINE nl_set_move_cd_y ( id_id , move_cd_y )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: move_cd_y
  INTEGER id_id
  model_config_rec%move_cd_y(id_id) = move_cd_y
  RETURN
END SUBROUTINE nl_set_move_cd_y
SUBROUTINE nl_set_swap_x ( id_id , swap_x )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: swap_x
  INTEGER id_id
  model_config_rec%swap_x(id_id) = swap_x
  RETURN
END SUBROUTINE nl_set_swap_x
SUBROUTINE nl_set_swap_y ( id_id , swap_y )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: swap_y
  INTEGER id_id
  model_config_rec%swap_y(id_id) = swap_y
  RETURN
END SUBROUTINE nl_set_swap_y
SUBROUTINE nl_set_cycle_x ( id_id , cycle_x )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: cycle_x
  INTEGER id_id
  model_config_rec%cycle_x(id_id) = cycle_x
  RETURN
END SUBROUTINE nl_set_cycle_x
SUBROUTINE nl_set_cycle_y ( id_id , cycle_y )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: cycle_y
  INTEGER id_id
  model_config_rec%cycle_y(id_id) = cycle_y
  RETURN
END SUBROUTINE nl_set_cycle_y
SUBROUTINE nl_set_reorder_mesh ( id_id , reorder_mesh )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: reorder_mesh
  INTEGER id_id
  model_config_rec%reorder_mesh = reorder_mesh
  RETURN
END SUBROUTINE nl_set_reorder_mesh
SUBROUTINE nl_set_perturb_input ( id_id , perturb_input )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: perturb_input
  INTEGER id_id
  model_config_rec%perturb_input = perturb_input
  RETURN
END SUBROUTINE nl_set_perturb_input
SUBROUTINE nl_set_eta_levels ( id_id , eta_levels )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: eta_levels
  INTEGER id_id
  model_config_rec%eta_levels(id_id) = eta_levels
  RETURN
END SUBROUTINE nl_set_eta_levels
SUBROUTINE nl_set_auto_levels_opt ( id_id , auto_levels_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: auto_levels_opt
  INTEGER id_id
  model_config_rec%auto_levels_opt = auto_levels_opt
  RETURN
END SUBROUTINE nl_set_auto_levels_opt
SUBROUTINE nl_set_max_dz ( id_id , max_dz )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: max_dz
  INTEGER id_id
  model_config_rec%max_dz = max_dz
  RETURN
END SUBROUTINE nl_set_max_dz
SUBROUTINE nl_set_dzbot ( id_id , dzbot )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dzbot
  INTEGER id_id
  model_config_rec%dzbot = dzbot
  RETURN
END SUBROUTINE nl_set_dzbot
SUBROUTINE nl_set_dzstretch_s ( id_id , dzstretch_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dzstretch_s
  INTEGER id_id
  model_config_rec%dzstretch_s = dzstretch_s
  RETURN
END SUBROUTINE nl_set_dzstretch_s
SUBROUTINE nl_set_dzstretch_u ( id_id , dzstretch_u )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: dzstretch_u
  INTEGER id_id
  model_config_rec%dzstretch_u = dzstretch_u
  RETURN
END SUBROUTINE nl_set_dzstretch_u
SUBROUTINE nl_set_ocean_levels ( id_id , ocean_levels )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ocean_levels
  INTEGER id_id
  model_config_rec%ocean_levels = ocean_levels
  RETURN
END SUBROUTINE nl_set_ocean_levels
SUBROUTINE nl_set_ocean_z ( id_id , ocean_z )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ocean_z
  INTEGER id_id
  model_config_rec%ocean_z(id_id) = ocean_z
  RETURN
END SUBROUTINE nl_set_ocean_z
SUBROUTINE nl_set_ocean_t ( id_id , ocean_t )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ocean_t
  INTEGER id_id
  model_config_rec%ocean_t(id_id) = ocean_t
  RETURN
END SUBROUTINE nl_set_ocean_t
SUBROUTINE nl_set_ocean_s ( id_id , ocean_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ocean_s
  INTEGER id_id
  model_config_rec%ocean_s(id_id) = ocean_s
  RETURN
END SUBROUTINE nl_set_ocean_s
SUBROUTINE nl_set_num_traj ( id_id , num_traj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_traj
  INTEGER id_id
  model_config_rec%num_traj = num_traj
  RETURN
END SUBROUTINE nl_set_num_traj
SUBROUTINE nl_set_max_ts_level ( id_id , max_ts_level )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: max_ts_level
  INTEGER id_id
  model_config_rec%max_ts_level = max_ts_level
  RETURN
END SUBROUTINE nl_set_max_ts_level
SUBROUTINE nl_set_track_loc_in ( id_id , track_loc_in )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: track_loc_in
  INTEGER id_id
  model_config_rec%track_loc_in = track_loc_in
  RETURN
END SUBROUTINE nl_set_track_loc_in
SUBROUTINE nl_set_num_ext_model_couple_dom ( id_id , num_ext_model_couple_dom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_ext_model_couple_dom
  INTEGER id_id
  model_config_rec%num_ext_model_couple_dom = num_ext_model_couple_dom
  RETURN
END SUBROUTINE nl_set_num_ext_model_couple_dom
SUBROUTINE nl_set_insert_bogus_storm ( id_id , insert_bogus_storm )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: insert_bogus_storm
  INTEGER id_id
  model_config_rec%insert_bogus_storm = insert_bogus_storm
  RETURN
END SUBROUTINE nl_set_insert_bogus_storm
SUBROUTINE nl_set_remove_storm ( id_id , remove_storm )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: remove_storm
  INTEGER id_id
  model_config_rec%remove_storm = remove_storm
  RETURN
END SUBROUTINE nl_set_remove_storm
SUBROUTINE nl_set_num_storm ( id_id , num_storm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_storm
  INTEGER id_id
  model_config_rec%num_storm = num_storm
  RETURN
END SUBROUTINE nl_set_num_storm
SUBROUTINE nl_set_latc_loc ( id_id , latc_loc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: latc_loc
  INTEGER id_id
  model_config_rec%latc_loc(id_id) = latc_loc
  RETURN
END SUBROUTINE nl_set_latc_loc
SUBROUTINE nl_set_lonc_loc ( id_id , lonc_loc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: lonc_loc
  INTEGER id_id
  model_config_rec%lonc_loc(id_id) = lonc_loc
  RETURN
END SUBROUTINE nl_set_lonc_loc
SUBROUTINE nl_set_vmax_meters_per_second ( id_id , vmax_meters_per_second )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: vmax_meters_per_second
  INTEGER id_id
  model_config_rec%vmax_meters_per_second(id_id) = vmax_meters_per_second
  RETURN
END SUBROUTINE nl_set_vmax_meters_per_second
SUBROUTINE nl_set_rmax ( id_id , rmax )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: rmax
  INTEGER id_id
  model_config_rec%rmax(id_id) = rmax
  RETURN
END SUBROUTINE nl_set_rmax
SUBROUTINE nl_set_vmax_ratio ( id_id , vmax_ratio )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: vmax_ratio
  INTEGER id_id
  model_config_rec%vmax_ratio(id_id) = vmax_ratio
  RETURN
END SUBROUTINE nl_set_vmax_ratio
SUBROUTINE nl_set_rankine_lid ( id_id , rankine_lid )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: rankine_lid
  INTEGER id_id
  model_config_rec%rankine_lid = rankine_lid
  RETURN
END SUBROUTINE nl_set_rankine_lid
SUBROUTINE nl_set_physics_suite ( id_id , physics_suite )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: physics_suite
  INTEGER id_id
  model_config_rec%physics_suite = trim(physics_suite)
  RETURN
END SUBROUTINE nl_set_physics_suite
SUBROUTINE nl_set_force_read_thompson ( id_id , force_read_thompson )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: force_read_thompson
  INTEGER id_id
  model_config_rec%force_read_thompson = force_read_thompson
  RETURN
END SUBROUTINE nl_set_force_read_thompson
SUBROUTINE nl_set_write_thompson_tables ( id_id , write_thompson_tables )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: write_thompson_tables
  INTEGER id_id
  model_config_rec%write_thompson_tables = write_thompson_tables
  RETURN
END SUBROUTINE nl_set_write_thompson_tables
SUBROUTINE nl_set_mp_physics ( id_id , mp_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mp_physics
  INTEGER id_id
  model_config_rec%mp_physics(id_id) = mp_physics
  RETURN
END SUBROUTINE nl_set_mp_physics
SUBROUTINE nl_set_nssl_cccn ( id_id , nssl_cccn )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_cccn
  INTEGER id_id
  model_config_rec%nssl_cccn(id_id) = nssl_cccn
  RETURN
END SUBROUTINE nl_set_nssl_cccn
SUBROUTINE nl_set_nssl_alphah ( id_id , nssl_alphah )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_alphah
  INTEGER id_id
  model_config_rec%nssl_alphah(id_id) = nssl_alphah
  RETURN
END SUBROUTINE nl_set_nssl_alphah
SUBROUTINE nl_set_nssl_alphahl ( id_id , nssl_alphahl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_alphahl
  INTEGER id_id
  model_config_rec%nssl_alphahl(id_id) = nssl_alphahl
  RETURN
END SUBROUTINE nl_set_nssl_alphahl
SUBROUTINE nl_set_nssl_cnoh ( id_id , nssl_cnoh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_cnoh
  INTEGER id_id
  model_config_rec%nssl_cnoh(id_id) = nssl_cnoh
  RETURN
END SUBROUTINE nl_set_nssl_cnoh
SUBROUTINE nl_set_nssl_cnohl ( id_id , nssl_cnohl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_cnohl
  INTEGER id_id
  model_config_rec%nssl_cnohl(id_id) = nssl_cnohl
  RETURN
END SUBROUTINE nl_set_nssl_cnohl
SUBROUTINE nl_set_nssl_cnor ( id_id , nssl_cnor )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_cnor
  INTEGER id_id
  model_config_rec%nssl_cnor(id_id) = nssl_cnor
  RETURN
END SUBROUTINE nl_set_nssl_cnor
SUBROUTINE nl_set_nssl_cnos ( id_id , nssl_cnos )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_cnos
  INTEGER id_id
  model_config_rec%nssl_cnos(id_id) = nssl_cnos
  RETURN
END SUBROUTINE nl_set_nssl_cnos
SUBROUTINE nl_set_nssl_rho_qh ( id_id , nssl_rho_qh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_rho_qh
  INTEGER id_id
  model_config_rec%nssl_rho_qh(id_id) = nssl_rho_qh
  RETURN
END SUBROUTINE nl_set_nssl_rho_qh
SUBROUTINE nl_set_nssl_rho_qhl ( id_id , nssl_rho_qhl )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_rho_qhl
  INTEGER id_id
  model_config_rec%nssl_rho_qhl(id_id) = nssl_rho_qhl
  RETURN
END SUBROUTINE nl_set_nssl_rho_qhl
SUBROUTINE nl_set_nssl_rho_qs ( id_id , nssl_rho_qs )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: nssl_rho_qs
  INTEGER id_id
  model_config_rec%nssl_rho_qs(id_id) = nssl_rho_qs
  RETURN
END SUBROUTINE nl_set_nssl_rho_qs
SUBROUTINE nl_set_ccnty ( id_id , ccnty )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ccnty
  INTEGER id_id
  model_config_rec%ccnty = ccnty
  RETURN
END SUBROUTINE nl_set_ccnty
SUBROUTINE nl_set_nudge_lightning ( id_id , nudge_lightning )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nudge_lightning
  INTEGER id_id
  model_config_rec%nudge_lightning(id_id) = nudge_lightning
  RETURN
END SUBROUTINE nl_set_nudge_lightning
SUBROUTINE nl_set_nudge_light_times ( id_id , nudge_light_times )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nudge_light_times
  INTEGER id_id
  model_config_rec%nudge_light_times(id_id) = nudge_light_times
  RETURN
END SUBROUTINE nl_set_nudge_light_times
SUBROUTINE nl_set_nudge_light_timee ( id_id , nudge_light_timee )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nudge_light_timee
  INTEGER id_id
  model_config_rec%nudge_light_timee(id_id) = nudge_light_timee
  RETURN
END SUBROUTINE nl_set_nudge_light_timee
SUBROUTINE nl_set_nudge_light_int ( id_id , nudge_light_int )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nudge_light_int
  INTEGER id_id
  model_config_rec%nudge_light_int(id_id) = nudge_light_int
  RETURN
END SUBROUTINE nl_set_nudge_light_int
SUBROUTINE nl_set_path_to_files ( id_id , path_to_files )
  USE module_configure, ONLY : model_config_rec
  character*256 , INTENT(IN) :: path_to_files
  INTEGER id_id
  model_config_rec%path_to_files = trim(path_to_files)
  RETURN
END SUBROUTINE nl_set_path_to_files
SUBROUTINE nl_set_gsfcgce_hail ( id_id , gsfcgce_hail )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gsfcgce_hail
  INTEGER id_id
  model_config_rec%gsfcgce_hail = gsfcgce_hail
  RETURN
END SUBROUTINE nl_set_gsfcgce_hail
SUBROUTINE nl_set_gsfcgce_2ice ( id_id , gsfcgce_2ice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: gsfcgce_2ice
  INTEGER id_id
  model_config_rec%gsfcgce_2ice = gsfcgce_2ice
  RETURN
END SUBROUTINE nl_set_gsfcgce_2ice
SUBROUTINE nl_set_progn ( id_id , progn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: progn
  INTEGER id_id
  model_config_rec%progn(id_id) = progn
  RETURN
END SUBROUTINE nl_set_progn
SUBROUTINE nl_set_accum_mode ( id_id , accum_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: accum_mode
  INTEGER id_id
  model_config_rec%accum_mode = accum_mode
  RETURN
END SUBROUTINE nl_set_accum_mode
SUBROUTINE nl_set_aitken_mode ( id_id , aitken_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aitken_mode
  INTEGER id_id
  model_config_rec%aitken_mode = aitken_mode
  RETURN
END SUBROUTINE nl_set_aitken_mode
SUBROUTINE nl_set_coarse_mode ( id_id , coarse_mode )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: coarse_mode
  INTEGER id_id
  model_config_rec%coarse_mode = coarse_mode
  RETURN
END SUBROUTINE nl_set_coarse_mode
SUBROUTINE nl_set_do_radar_ref ( id_id , do_radar_ref )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: do_radar_ref
  INTEGER id_id
  model_config_rec%do_radar_ref = do_radar_ref
  RETURN
END SUBROUTINE nl_set_do_radar_ref
SUBROUTINE nl_set_compute_radar_ref ( id_id , compute_radar_ref )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: compute_radar_ref
  INTEGER id_id
  model_config_rec%compute_radar_ref = compute_radar_ref
  RETURN
END SUBROUTINE nl_set_compute_radar_ref
SUBROUTINE nl_set_ra_lw_physics ( id_id , ra_lw_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ra_lw_physics
  INTEGER id_id
  model_config_rec%ra_lw_physics(id_id) = ra_lw_physics
  RETURN
END SUBROUTINE nl_set_ra_lw_physics
SUBROUTINE nl_set_ra_sw_physics ( id_id , ra_sw_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ra_sw_physics
  INTEGER id_id
  model_config_rec%ra_sw_physics(id_id) = ra_sw_physics
  RETURN
END SUBROUTINE nl_set_ra_sw_physics
SUBROUTINE nl_set_ra_sw_eclipse ( id_id , ra_sw_eclipse )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ra_sw_eclipse
  INTEGER id_id
  model_config_rec%ra_sw_eclipse = ra_sw_eclipse
  RETURN
END SUBROUTINE nl_set_ra_sw_eclipse
SUBROUTINE nl_set_ghg_input ( id_id , ghg_input )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ghg_input
  INTEGER id_id
  model_config_rec%ghg_input = ghg_input
  RETURN
END SUBROUTINE nl_set_ghg_input
SUBROUTINE nl_set_radt ( id_id , radt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: radt
  INTEGER id_id
  model_config_rec%radt(id_id) = radt
  RETURN
END SUBROUTINE nl_set_radt
SUBROUTINE nl_set_naer ( id_id , naer )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: naer
  INTEGER id_id
  model_config_rec%naer(id_id) = naer
  RETURN
END SUBROUTINE nl_set_naer
SUBROUTINE nl_set_sf_sfclay_physics ( id_id , sf_sfclay_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_sfclay_physics
  INTEGER id_id
  model_config_rec%sf_sfclay_physics(id_id) = sf_sfclay_physics
  RETURN
END SUBROUTINE nl_set_sf_sfclay_physics
SUBROUTINE nl_set_sf_surf_irr_scheme ( id_id , sf_surf_irr_scheme )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_surf_irr_scheme
  INTEGER id_id
  model_config_rec%sf_surf_irr_scheme(id_id) = sf_surf_irr_scheme
  RETURN
END SUBROUTINE nl_set_sf_surf_irr_scheme
SUBROUTINE nl_set_sf_surf_irr_alloc ( id_id , sf_surf_irr_alloc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_surf_irr_alloc
  INTEGER id_id
  model_config_rec%sf_surf_irr_alloc = sf_surf_irr_alloc
  RETURN
END SUBROUTINE nl_set_sf_surf_irr_alloc
SUBROUTINE nl_set_irr_daily_amount ( id_id , irr_daily_amount )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: irr_daily_amount
  INTEGER id_id
  model_config_rec%irr_daily_amount(id_id) = irr_daily_amount
  RETURN
END SUBROUTINE nl_set_irr_daily_amount
SUBROUTINE nl_set_irr_start_hour ( id_id , irr_start_hour )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_start_hour
  INTEGER id_id
  model_config_rec%irr_start_hour(id_id) = irr_start_hour
  RETURN
END SUBROUTINE nl_set_irr_start_hour
SUBROUTINE nl_set_irr_num_hours ( id_id , irr_num_hours )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_num_hours
  INTEGER id_id
  model_config_rec%irr_num_hours(id_id) = irr_num_hours
  RETURN
END SUBROUTINE nl_set_irr_num_hours
SUBROUTINE nl_set_irr_start_julianday ( id_id , irr_start_julianday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_start_julianday
  INTEGER id_id
  model_config_rec%irr_start_julianday(id_id) = irr_start_julianday
  RETURN
END SUBROUTINE nl_set_irr_start_julianday
SUBROUTINE nl_set_irr_end_julianday ( id_id , irr_end_julianday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_end_julianday
  INTEGER id_id
  model_config_rec%irr_end_julianday(id_id) = irr_end_julianday
  RETURN
END SUBROUTINE nl_set_irr_end_julianday
SUBROUTINE nl_set_irr_freq ( id_id , irr_freq )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_freq
  INTEGER id_id
  model_config_rec%irr_freq(id_id) = irr_freq
  RETURN
END SUBROUTINE nl_set_irr_freq
SUBROUTINE nl_set_irr_ph ( id_id , irr_ph )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: irr_ph
  INTEGER id_id
  model_config_rec%irr_ph(id_id) = irr_ph
  RETURN
END SUBROUTINE nl_set_irr_ph
SUBROUTINE nl_set_sf_surface_physics ( id_id , sf_surface_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_surface_physics
  INTEGER id_id
  model_config_rec%sf_surface_physics(id_id) = sf_surface_physics
  RETURN
END SUBROUTINE nl_set_sf_surface_physics
SUBROUTINE nl_set_bl_pbl_physics ( id_id , bl_pbl_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_pbl_physics
  INTEGER id_id
  model_config_rec%bl_pbl_physics(id_id) = bl_pbl_physics
  RETURN
END SUBROUTINE nl_set_bl_pbl_physics
SUBROUTINE nl_set_bl_mynn_tkebudget ( id_id , bl_mynn_tkebudget )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_tkebudget
  INTEGER id_id
  model_config_rec%bl_mynn_tkebudget(id_id) = bl_mynn_tkebudget
  RETURN
END SUBROUTINE nl_set_bl_mynn_tkebudget
SUBROUTINE nl_set_ysu_topdown_pblmix ( id_id , ysu_topdown_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ysu_topdown_pblmix
  INTEGER id_id
  model_config_rec%ysu_topdown_pblmix = ysu_topdown_pblmix
  RETURN
END SUBROUTINE nl_set_ysu_topdown_pblmix
SUBROUTINE nl_set_shinhong_tke_diag ( id_id , shinhong_tke_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shinhong_tke_diag
  INTEGER id_id
  model_config_rec%shinhong_tke_diag(id_id) = shinhong_tke_diag
  RETURN
END SUBROUTINE nl_set_shinhong_tke_diag
SUBROUTINE nl_set_bl_mynn_tkeadvect ( id_id , bl_mynn_tkeadvect )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: bl_mynn_tkeadvect
  INTEGER id_id
  model_config_rec%bl_mynn_tkeadvect(id_id) = bl_mynn_tkeadvect
  RETURN
END SUBROUTINE nl_set_bl_mynn_tkeadvect
SUBROUTINE nl_set_bl_mynn_cloudpdf ( id_id , bl_mynn_cloudpdf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_cloudpdf
  INTEGER id_id
  model_config_rec%bl_mynn_cloudpdf = bl_mynn_cloudpdf
  RETURN
END SUBROUTINE nl_set_bl_mynn_cloudpdf
SUBROUTINE nl_set_bl_mynn_mixlength ( id_id , bl_mynn_mixlength )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_mixlength
  INTEGER id_id
  model_config_rec%bl_mynn_mixlength = bl_mynn_mixlength
  RETURN
END SUBROUTINE nl_set_bl_mynn_mixlength
SUBROUTINE nl_set_bl_mynn_edmf ( id_id , bl_mynn_edmf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_edmf
  INTEGER id_id
  model_config_rec%bl_mynn_edmf(id_id) = bl_mynn_edmf
  RETURN
END SUBROUTINE nl_set_bl_mynn_edmf
SUBROUTINE nl_set_bl_mynn_edmf_mom ( id_id , bl_mynn_edmf_mom )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_edmf_mom
  INTEGER id_id
  model_config_rec%bl_mynn_edmf_mom(id_id) = bl_mynn_edmf_mom
  RETURN
END SUBROUTINE nl_set_bl_mynn_edmf_mom
SUBROUTINE nl_set_bl_mynn_edmf_tke ( id_id , bl_mynn_edmf_tke )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_edmf_tke
  INTEGER id_id
  model_config_rec%bl_mynn_edmf_tke(id_id) = bl_mynn_edmf_tke
  RETURN
END SUBROUTINE nl_set_bl_mynn_edmf_tke
SUBROUTINE nl_set_bl_mynn_mixscalars ( id_id , bl_mynn_mixscalars )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_mixscalars
  INTEGER id_id
  model_config_rec%bl_mynn_mixscalars(id_id) = bl_mynn_mixscalars
  RETURN
END SUBROUTINE nl_set_bl_mynn_mixscalars
SUBROUTINE nl_set_bl_mynn_output ( id_id , bl_mynn_output )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_output
  INTEGER id_id
  model_config_rec%bl_mynn_output(id_id) = bl_mynn_output
  RETURN
END SUBROUTINE nl_set_bl_mynn_output
SUBROUTINE nl_set_bl_mynn_cloudmix ( id_id , bl_mynn_cloudmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_cloudmix
  INTEGER id_id
  model_config_rec%bl_mynn_cloudmix(id_id) = bl_mynn_cloudmix
  RETURN
END SUBROUTINE nl_set_bl_mynn_cloudmix
SUBROUTINE nl_set_bl_mynn_mixqt ( id_id , bl_mynn_mixqt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bl_mynn_mixqt
  INTEGER id_id
  model_config_rec%bl_mynn_mixqt(id_id) = bl_mynn_mixqt
  RETURN
END SUBROUTINE nl_set_bl_mynn_mixqt
SUBROUTINE nl_set_icloud_bl ( id_id , icloud_bl )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: icloud_bl
  INTEGER id_id
  model_config_rec%icloud_bl = icloud_bl
  RETURN
END SUBROUTINE nl_set_icloud_bl
SUBROUTINE nl_set_mfshconv ( id_id , mfshconv )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mfshconv
  INTEGER id_id
  model_config_rec%mfshconv(id_id) = mfshconv
  RETURN
END SUBROUTINE nl_set_mfshconv
SUBROUTINE nl_set_sf_urban_physics ( id_id , sf_urban_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_urban_physics
  INTEGER id_id
  model_config_rec%sf_urban_physics(id_id) = sf_urban_physics
  RETURN
END SUBROUTINE nl_set_sf_urban_physics
SUBROUTINE nl_set_bldt ( id_id , bldt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: bldt
  INTEGER id_id
  model_config_rec%bldt(id_id) = bldt
  RETURN
END SUBROUTINE nl_set_bldt
SUBROUTINE nl_set_cu_physics ( id_id , cu_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cu_physics
  INTEGER id_id
  model_config_rec%cu_physics(id_id) = cu_physics
  RETURN
END SUBROUTINE nl_set_cu_physics
SUBROUTINE nl_set_shcu_physics ( id_id , shcu_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shcu_physics
  INTEGER id_id
  model_config_rec%shcu_physics(id_id) = shcu_physics
  RETURN
END SUBROUTINE nl_set_shcu_physics
SUBROUTINE nl_set_cu_diag ( id_id , cu_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cu_diag
  INTEGER id_id
  model_config_rec%cu_diag(id_id) = cu_diag
  RETURN
END SUBROUTINE nl_set_cu_diag
SUBROUTINE nl_set_kf_edrates ( id_id , kf_edrates )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kf_edrates
  INTEGER id_id
  model_config_rec%kf_edrates(id_id) = kf_edrates
  RETURN
END SUBROUTINE nl_set_kf_edrates
SUBROUTINE nl_set_kfeta_trigger ( id_id , kfeta_trigger )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: kfeta_trigger
  INTEGER id_id
  model_config_rec%kfeta_trigger = kfeta_trigger
  RETURN
END SUBROUTINE nl_set_kfeta_trigger
SUBROUTINE nl_set_nsas_dx_factor ( id_id , nsas_dx_factor )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: nsas_dx_factor
  INTEGER id_id
  model_config_rec%nsas_dx_factor = nsas_dx_factor
  RETURN
END SUBROUTINE nl_set_nsas_dx_factor
SUBROUTINE nl_set_cudt ( id_id , cudt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: cudt
  INTEGER id_id
  model_config_rec%cudt(id_id) = cudt
  RETURN
END SUBROUTINE nl_set_cudt
SUBROUTINE nl_set_gsmdt ( id_id , gsmdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: gsmdt
  INTEGER id_id
  model_config_rec%gsmdt(id_id) = gsmdt
  RETURN
END SUBROUTINE nl_set_gsmdt
SUBROUTINE nl_set_isfflx ( id_id , isfflx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: isfflx
  INTEGER id_id
  model_config_rec%isfflx = isfflx
  RETURN
END SUBROUTINE nl_set_isfflx
SUBROUTINE nl_set_ifsnow ( id_id , ifsnow )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ifsnow
  INTEGER id_id
  model_config_rec%ifsnow = ifsnow
  RETURN
END SUBROUTINE nl_set_ifsnow
SUBROUTINE nl_set_icloud ( id_id , icloud )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: icloud
  INTEGER id_id
  model_config_rec%icloud = icloud
  RETURN
END SUBROUTINE nl_set_icloud
SUBROUTINE nl_set_cldovrlp ( id_id , cldovrlp )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cldovrlp
  INTEGER id_id
  model_config_rec%cldovrlp = cldovrlp
  RETURN
END SUBROUTINE nl_set_cldovrlp
SUBROUTINE nl_set_idcor ( id_id , idcor )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: idcor
  INTEGER id_id
  model_config_rec%idcor = idcor
  RETURN
END SUBROUTINE nl_set_idcor
SUBROUTINE nl_set_ideal_xland ( id_id , ideal_xland )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ideal_xland
  INTEGER id_id
  model_config_rec%ideal_xland = ideal_xland
  RETURN
END SUBROUTINE nl_set_ideal_xland
SUBROUTINE nl_set_swrad_scat ( id_id , swrad_scat )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: swrad_scat
  INTEGER id_id
  model_config_rec%swrad_scat = swrad_scat
  RETURN
END SUBROUTINE nl_set_swrad_scat
SUBROUTINE nl_set_surface_input_source ( id_id , surface_input_source )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: surface_input_source
  INTEGER id_id
  model_config_rec%surface_input_source = surface_input_source
  RETURN
END SUBROUTINE nl_set_surface_input_source
SUBROUTINE nl_set_num_soil_layers ( id_id , num_soil_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_soil_layers
  INTEGER id_id
  model_config_rec%num_soil_layers = num_soil_layers
  RETURN
END SUBROUTINE nl_set_num_soil_layers
SUBROUTINE nl_set_num_pft_clm ( id_id , num_pft_clm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_pft_clm
  INTEGER id_id
  model_config_rec%num_pft_clm = num_pft_clm
  RETURN
END SUBROUTINE nl_set_num_pft_clm
SUBROUTINE nl_set_input_pft ( id_id , input_pft )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: input_pft
  INTEGER id_id
  model_config_rec%input_pft = input_pft
  RETURN
END SUBROUTINE nl_set_input_pft
SUBROUTINE nl_set_maxpatch ( id_id , maxpatch )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: maxpatch
  INTEGER id_id
  model_config_rec%maxpatch = maxpatch
  RETURN
END SUBROUTINE nl_set_maxpatch
SUBROUTINE nl_set_num_snow_layers ( id_id , num_snow_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_snow_layers
  INTEGER id_id
  model_config_rec%num_snow_layers = num_snow_layers
  RETURN
END SUBROUTINE nl_set_num_snow_layers
SUBROUTINE nl_set_num_snso_layers ( id_id , num_snso_layers )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_snso_layers
  INTEGER id_id
  model_config_rec%num_snso_layers = num_snso_layers
  RETURN
END SUBROUTINE nl_set_num_snso_layers
SUBROUTINE nl_set_num_urban_ndm ( id_id , num_urban_ndm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_ndm
  INTEGER id_id
  model_config_rec%num_urban_ndm = num_urban_ndm
  RETURN
END SUBROUTINE nl_set_num_urban_ndm
SUBROUTINE nl_set_num_urban_ng ( id_id , num_urban_ng )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_ng
  INTEGER id_id
  model_config_rec%num_urban_ng = num_urban_ng
  RETURN
END SUBROUTINE nl_set_num_urban_ng
SUBROUTINE nl_set_num_urban_nwr ( id_id , num_urban_nwr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_nwr
  INTEGER id_id
  model_config_rec%num_urban_nwr = num_urban_nwr
  RETURN
END SUBROUTINE nl_set_num_urban_nwr
SUBROUTINE nl_set_num_urban_ngb ( id_id , num_urban_ngb )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_ngb
  INTEGER id_id
  model_config_rec%num_urban_ngb = num_urban_ngb
  RETURN
END SUBROUTINE nl_set_num_urban_ngb
SUBROUTINE nl_set_num_urban_nf ( id_id , num_urban_nf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_nf
  INTEGER id_id
  model_config_rec%num_urban_nf = num_urban_nf
  RETURN
END SUBROUTINE nl_set_num_urban_nf
SUBROUTINE nl_set_num_urban_nz ( id_id , num_urban_nz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_nz
  INTEGER id_id
  model_config_rec%num_urban_nz = num_urban_nz
  RETURN
END SUBROUTINE nl_set_num_urban_nz
SUBROUTINE nl_set_num_urban_nbui ( id_id , num_urban_nbui )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_nbui
  INTEGER id_id
  model_config_rec%num_urban_nbui = num_urban_nbui
  RETURN
END SUBROUTINE nl_set_num_urban_nbui
SUBROUTINE nl_set_num_urban_ngr ( id_id , num_urban_ngr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_ngr
  INTEGER id_id
  model_config_rec%num_urban_ngr = num_urban_ngr
  RETURN
END SUBROUTINE nl_set_num_urban_ngr
SUBROUTINE nl_set_urban_map_zrd ( id_id , urban_map_zrd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_zrd
  INTEGER id_id
  model_config_rec%urban_map_zrd = urban_map_zrd
  RETURN
END SUBROUTINE nl_set_urban_map_zrd
SUBROUTINE nl_set_urban_map_zwd ( id_id , urban_map_zwd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_zwd
  INTEGER id_id
  model_config_rec%urban_map_zwd = urban_map_zwd
  RETURN
END SUBROUTINE nl_set_urban_map_zwd
SUBROUTINE nl_set_urban_map_gd ( id_id , urban_map_gd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_gd
  INTEGER id_id
  model_config_rec%urban_map_gd = urban_map_gd
  RETURN
END SUBROUTINE nl_set_urban_map_gd
SUBROUTINE nl_set_urban_map_zd ( id_id , urban_map_zd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_zd
  INTEGER id_id
  model_config_rec%urban_map_zd = urban_map_zd
  RETURN
END SUBROUTINE nl_set_urban_map_zd
SUBROUTINE nl_set_urban_map_zdf ( id_id , urban_map_zdf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_zdf
  INTEGER id_id
  model_config_rec%urban_map_zdf = urban_map_zdf
  RETURN
END SUBROUTINE nl_set_urban_map_zdf
SUBROUTINE nl_set_urban_map_bd ( id_id , urban_map_bd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_bd
  INTEGER id_id
  model_config_rec%urban_map_bd = urban_map_bd
  RETURN
END SUBROUTINE nl_set_urban_map_bd
SUBROUTINE nl_set_urban_map_wd ( id_id , urban_map_wd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_wd
  INTEGER id_id
  model_config_rec%urban_map_wd = urban_map_wd
  RETURN
END SUBROUTINE nl_set_urban_map_wd
SUBROUTINE nl_set_urban_map_gbd ( id_id , urban_map_gbd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_gbd
  INTEGER id_id
  model_config_rec%urban_map_gbd = urban_map_gbd
  RETURN
END SUBROUTINE nl_set_urban_map_gbd
SUBROUTINE nl_set_urban_map_fbd ( id_id , urban_map_fbd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_fbd
  INTEGER id_id
  model_config_rec%urban_map_fbd = urban_map_fbd
  RETURN
END SUBROUTINE nl_set_urban_map_fbd
SUBROUTINE nl_set_urban_map_zgrd ( id_id , urban_map_zgrd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: urban_map_zgrd
  INTEGER id_id
  model_config_rec%urban_map_zgrd = urban_map_zgrd
  RETURN
END SUBROUTINE nl_set_urban_map_zgrd
SUBROUTINE nl_set_num_urban_hi ( id_id , num_urban_hi )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_urban_hi
  INTEGER id_id
  model_config_rec%num_urban_hi = num_urban_hi
  RETURN
END SUBROUTINE nl_set_num_urban_hi
SUBROUTINE nl_set_num_months ( id_id , num_months )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_months
  INTEGER id_id
  model_config_rec%num_months = num_months
  RETURN
END SUBROUTINE nl_set_num_months
SUBROUTINE nl_set_sf_surface_mosaic ( id_id , sf_surface_mosaic )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_surface_mosaic
  INTEGER id_id
  model_config_rec%sf_surface_mosaic = sf_surface_mosaic
  RETURN
END SUBROUTINE nl_set_sf_surface_mosaic
SUBROUTINE nl_set_mosaic_cat ( id_id , mosaic_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mosaic_cat
  INTEGER id_id
  model_config_rec%mosaic_cat = mosaic_cat
  RETURN
END SUBROUTINE nl_set_mosaic_cat
SUBROUTINE nl_set_mosaic_cat_soil ( id_id , mosaic_cat_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mosaic_cat_soil
  INTEGER id_id
  model_config_rec%mosaic_cat_soil = mosaic_cat_soil
  RETURN
END SUBROUTINE nl_set_mosaic_cat_soil
SUBROUTINE nl_set_mosaic_lu ( id_id , mosaic_lu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mosaic_lu
  INTEGER id_id
  model_config_rec%mosaic_lu = mosaic_lu
  RETURN
END SUBROUTINE nl_set_mosaic_lu
SUBROUTINE nl_set_mosaic_soil ( id_id , mosaic_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mosaic_soil
  INTEGER id_id
  model_config_rec%mosaic_soil = mosaic_soil
  RETURN
END SUBROUTINE nl_set_mosaic_soil
SUBROUTINE nl_set_flag_sm_adj ( id_id , flag_sm_adj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: flag_sm_adj
  INTEGER id_id
  model_config_rec%flag_sm_adj = flag_sm_adj
  RETURN
END SUBROUTINE nl_set_flag_sm_adj
SUBROUTINE nl_set_maxiens ( id_id , maxiens )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: maxiens
  INTEGER id_id
  model_config_rec%maxiens = maxiens
  RETURN
END SUBROUTINE nl_set_maxiens
SUBROUTINE nl_set_maxens ( id_id , maxens )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: maxens
  INTEGER id_id
  model_config_rec%maxens = maxens
  RETURN
END SUBROUTINE nl_set_maxens
SUBROUTINE nl_set_maxens2 ( id_id , maxens2 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: maxens2
  INTEGER id_id
  model_config_rec%maxens2 = maxens2
  RETURN
END SUBROUTINE nl_set_maxens2
SUBROUTINE nl_set_maxens3 ( id_id , maxens3 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: maxens3
  INTEGER id_id
  model_config_rec%maxens3 = maxens3
  RETURN
END SUBROUTINE nl_set_maxens3
SUBROUTINE nl_set_ensdim ( id_id , ensdim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ensdim
  INTEGER id_id
  model_config_rec%ensdim = ensdim
  RETURN
END SUBROUTINE nl_set_ensdim
SUBROUTINE nl_set_cugd_avedx ( id_id , cugd_avedx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cugd_avedx
  INTEGER id_id
  model_config_rec%cugd_avedx = cugd_avedx
  RETURN
END SUBROUTINE nl_set_cugd_avedx
SUBROUTINE nl_set_clos_choice ( id_id , clos_choice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: clos_choice
  INTEGER id_id
  model_config_rec%clos_choice = clos_choice
  RETURN
END SUBROUTINE nl_set_clos_choice
SUBROUTINE nl_set_imomentum ( id_id , imomentum )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: imomentum
  INTEGER id_id
  model_config_rec%imomentum = imomentum
  RETURN
END SUBROUTINE nl_set_imomentum
SUBROUTINE nl_set_ishallow ( id_id , ishallow )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ishallow
  INTEGER id_id
  model_config_rec%ishallow = ishallow
  RETURN
END SUBROUTINE nl_set_ishallow
SUBROUTINE nl_set_convtrans_avglen_m ( id_id , convtrans_avglen_m )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: convtrans_avglen_m
  INTEGER id_id
  model_config_rec%convtrans_avglen_m = convtrans_avglen_m
  RETURN
END SUBROUTINE nl_set_convtrans_avglen_m
SUBROUTINE nl_set_num_land_cat ( id_id , num_land_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_land_cat
  INTEGER id_id
  model_config_rec%num_land_cat = num_land_cat
  RETURN
END SUBROUTINE nl_set_num_land_cat
SUBROUTINE nl_set_use_wudapt_lcz ( id_id , use_wudapt_lcz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: use_wudapt_lcz
  INTEGER id_id
  model_config_rec%use_wudapt_lcz = use_wudapt_lcz
  RETURN
END SUBROUTINE nl_set_use_wudapt_lcz
SUBROUTINE nl_set_num_soil_cat ( id_id , num_soil_cat )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: num_soil_cat
  INTEGER id_id
  model_config_rec%num_soil_cat = num_soil_cat
  RETURN
END SUBROUTINE nl_set_num_soil_cat
SUBROUTINE nl_set_mp_zero_out ( id_id , mp_zero_out )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: mp_zero_out
  INTEGER id_id
  model_config_rec%mp_zero_out = mp_zero_out
  RETURN
END SUBROUTINE nl_set_mp_zero_out
SUBROUTINE nl_set_mp_zero_out_thresh ( id_id , mp_zero_out_thresh )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: mp_zero_out_thresh
  INTEGER id_id
  model_config_rec%mp_zero_out_thresh = mp_zero_out_thresh
  RETURN
END SUBROUTINE nl_set_mp_zero_out_thresh
SUBROUTINE nl_set_seaice_threshold ( id_id , seaice_threshold )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: seaice_threshold
  INTEGER id_id
  model_config_rec%seaice_threshold = seaice_threshold
  RETURN
END SUBROUTINE nl_set_seaice_threshold
SUBROUTINE nl_set_bmj_rad_feedback ( id_id , bmj_rad_feedback )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: bmj_rad_feedback
  INTEGER id_id
  model_config_rec%bmj_rad_feedback(id_id) = bmj_rad_feedback
  RETURN
END SUBROUTINE nl_set_bmj_rad_feedback
SUBROUTINE nl_set_sst_update ( id_id , sst_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sst_update
  INTEGER id_id
  model_config_rec%sst_update = sst_update
  RETURN
END SUBROUTINE nl_set_sst_update
SUBROUTINE nl_set_qna_update ( id_id , qna_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: qna_update
  INTEGER id_id
  model_config_rec%qna_update = qna_update
  RETURN
END SUBROUTINE nl_set_qna_update
SUBROUTINE nl_set_sst_skin ( id_id , sst_skin )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sst_skin
  INTEGER id_id
  model_config_rec%sst_skin = sst_skin
  RETURN
END SUBROUTINE nl_set_sst_skin
SUBROUTINE nl_set_tmn_update ( id_id , tmn_update )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tmn_update
  INTEGER id_id
  model_config_rec%tmn_update = tmn_update
  RETURN
END SUBROUTINE nl_set_tmn_update
SUBROUTINE nl_set_usemonalb ( id_id , usemonalb )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: usemonalb
  INTEGER id_id
  model_config_rec%usemonalb = usemonalb
  RETURN
END SUBROUTINE nl_set_usemonalb
SUBROUTINE nl_set_rdmaxalb ( id_id , rdmaxalb )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: rdmaxalb
  INTEGER id_id
  model_config_rec%rdmaxalb = rdmaxalb
  RETURN
END SUBROUTINE nl_set_rdmaxalb
SUBROUTINE nl_set_rdlai2d ( id_id , rdlai2d )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: rdlai2d
  INTEGER id_id
  model_config_rec%rdlai2d = rdlai2d
  RETURN
END SUBROUTINE nl_set_rdlai2d
SUBROUTINE nl_set_ua_phys ( id_id , ua_phys )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: ua_phys
  INTEGER id_id
  model_config_rec%ua_phys = ua_phys
  RETURN
END SUBROUTINE nl_set_ua_phys
SUBROUTINE nl_set_opt_thcnd ( id_id , opt_thcnd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_thcnd
  INTEGER id_id
  model_config_rec%opt_thcnd = opt_thcnd
  RETURN
END SUBROUTINE nl_set_opt_thcnd
SUBROUTINE nl_set_co2tf ( id_id , co2tf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: co2tf
  INTEGER id_id
  model_config_rec%co2tf = co2tf
  RETURN
END SUBROUTINE nl_set_co2tf
SUBROUTINE nl_set_ra_call_offset ( id_id , ra_call_offset )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: ra_call_offset
  INTEGER id_id
  model_config_rec%ra_call_offset = ra_call_offset
  RETURN
END SUBROUTINE nl_set_ra_call_offset
SUBROUTINE nl_set_cam_abs_freq_s ( id_id , cam_abs_freq_s )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: cam_abs_freq_s
  INTEGER id_id
  model_config_rec%cam_abs_freq_s = cam_abs_freq_s
  RETURN
END SUBROUTINE nl_set_cam_abs_freq_s
SUBROUTINE nl_set_levsiz ( id_id , levsiz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: levsiz
  INTEGER id_id
  model_config_rec%levsiz = levsiz
  RETURN
END SUBROUTINE nl_set_levsiz
SUBROUTINE nl_set_paerlev ( id_id , paerlev )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: paerlev
  INTEGER id_id
  model_config_rec%paerlev = paerlev
  RETURN
END SUBROUTINE nl_set_paerlev
SUBROUTINE nl_set_cam_abs_dim1 ( id_id , cam_abs_dim1 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cam_abs_dim1
  INTEGER id_id
  model_config_rec%cam_abs_dim1 = cam_abs_dim1
  RETURN
END SUBROUTINE nl_set_cam_abs_dim1
SUBROUTINE nl_set_cam_abs_dim2 ( id_id , cam_abs_dim2 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: cam_abs_dim2
  INTEGER id_id
  model_config_rec%cam_abs_dim2 = cam_abs_dim2
  RETURN
END SUBROUTINE nl_set_cam_abs_dim2
SUBROUTINE nl_set_lagday ( id_id , lagday )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: lagday
  INTEGER id_id
  model_config_rec%lagday = lagday
  RETURN
END SUBROUTINE nl_set_lagday
SUBROUTINE nl_set_no_src_types ( id_id , no_src_types )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: no_src_types
  INTEGER id_id
  model_config_rec%no_src_types = no_src_types
  RETURN
END SUBROUTINE nl_set_no_src_types
SUBROUTINE nl_set_alevsiz ( id_id , alevsiz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: alevsiz
  INTEGER id_id
  model_config_rec%alevsiz = alevsiz
  RETURN
END SUBROUTINE nl_set_alevsiz
SUBROUTINE nl_set_o3input ( id_id , o3input )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: o3input
  INTEGER id_id
  model_config_rec%o3input = o3input
  RETURN
END SUBROUTINE nl_set_o3input
SUBROUTINE nl_set_aer_opt ( id_id , aer_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_opt
  INTEGER id_id
  model_config_rec%aer_opt = aer_opt
  RETURN
END SUBROUTINE nl_set_aer_opt
SUBROUTINE nl_set_swint_opt ( id_id , swint_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: swint_opt
  INTEGER id_id
  model_config_rec%swint_opt = swint_opt
  RETURN
END SUBROUTINE nl_set_swint_opt
SUBROUTINE nl_set_aer_type ( id_id , aer_type )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_type
  INTEGER id_id
  model_config_rec%aer_type(id_id) = aer_type
  RETURN
END SUBROUTINE nl_set_aer_type
SUBROUTINE nl_set_aer_aod550_opt ( id_id , aer_aod550_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_aod550_opt
  INTEGER id_id
  model_config_rec%aer_aod550_opt(id_id) = aer_aod550_opt
  RETURN
END SUBROUTINE nl_set_aer_aod550_opt
SUBROUTINE nl_set_aer_angexp_opt ( id_id , aer_angexp_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_angexp_opt
  INTEGER id_id
  model_config_rec%aer_angexp_opt(id_id) = aer_angexp_opt
  RETURN
END SUBROUTINE nl_set_aer_angexp_opt
SUBROUTINE nl_set_aer_ssa_opt ( id_id , aer_ssa_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_ssa_opt
  INTEGER id_id
  model_config_rec%aer_ssa_opt(id_id) = aer_ssa_opt
  RETURN
END SUBROUTINE nl_set_aer_ssa_opt
SUBROUTINE nl_set_aer_asy_opt ( id_id , aer_asy_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_asy_opt
  INTEGER id_id
  model_config_rec%aer_asy_opt(id_id) = aer_asy_opt
  RETURN
END SUBROUTINE nl_set_aer_asy_opt
SUBROUTINE nl_set_aer_aod550_val ( id_id , aer_aod550_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aer_aod550_val
  INTEGER id_id
  model_config_rec%aer_aod550_val(id_id) = aer_aod550_val
  RETURN
END SUBROUTINE nl_set_aer_aod550_val
SUBROUTINE nl_set_aer_angexp_val ( id_id , aer_angexp_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aer_angexp_val
  INTEGER id_id
  model_config_rec%aer_angexp_val(id_id) = aer_angexp_val
  RETURN
END SUBROUTINE nl_set_aer_angexp_val
SUBROUTINE nl_set_aer_ssa_val ( id_id , aer_ssa_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aer_ssa_val
  INTEGER id_id
  model_config_rec%aer_ssa_val(id_id) = aer_ssa_val
  RETURN
END SUBROUTINE nl_set_aer_ssa_val
SUBROUTINE nl_set_aer_asy_val ( id_id , aer_asy_val )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aer_asy_val
  INTEGER id_id
  model_config_rec%aer_asy_val(id_id) = aer_asy_val
  RETURN
END SUBROUTINE nl_set_aer_asy_val
SUBROUTINE nl_set_cu_rad_feedback ( id_id , cu_rad_feedback )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: cu_rad_feedback
  INTEGER id_id
  model_config_rec%cu_rad_feedback(id_id) = cu_rad_feedback
  RETURN
END SUBROUTINE nl_set_cu_rad_feedback
SUBROUTINE nl_set_dust_emis ( id_id , dust_emis )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dust_emis
  INTEGER id_id
  model_config_rec%dust_emis = dust_emis
  RETURN
END SUBROUTINE nl_set_dust_emis
SUBROUTINE nl_set_erosion_dim ( id_id , erosion_dim )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: erosion_dim
  INTEGER id_id
  model_config_rec%erosion_dim = erosion_dim
  RETURN
END SUBROUTINE nl_set_erosion_dim
SUBROUTINE nl_set_no_src_types_cu ( id_id , no_src_types_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: no_src_types_cu
  INTEGER id_id
  model_config_rec%no_src_types_cu = no_src_types_cu
  RETURN
END SUBROUTINE nl_set_no_src_types_cu
SUBROUTINE nl_set_alevsiz_cu ( id_id , alevsiz_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: alevsiz_cu
  INTEGER id_id
  model_config_rec%alevsiz_cu = alevsiz_cu
  RETURN
END SUBROUTINE nl_set_alevsiz_cu
SUBROUTINE nl_set_aercu_opt ( id_id , aercu_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aercu_opt
  INTEGER id_id
  model_config_rec%aercu_opt = aercu_opt
  RETURN
END SUBROUTINE nl_set_aercu_opt
SUBROUTINE nl_set_aercu_fct ( id_id , aercu_fct )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: aercu_fct
  INTEGER id_id
  model_config_rec%aercu_fct = aercu_fct
  RETURN
END SUBROUTINE nl_set_aercu_fct
SUBROUTINE nl_set_aercu_used ( id_id , aercu_used )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aercu_used
  INTEGER id_id
  model_config_rec%aercu_used = aercu_used
  RETURN
END SUBROUTINE nl_set_aercu_used
SUBROUTINE nl_set_couple_farms ( id_id , couple_farms )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: couple_farms
  INTEGER id_id
  model_config_rec%couple_farms = couple_farms
  RETURN
END SUBROUTINE nl_set_couple_farms
SUBROUTINE nl_set_shallowcu_forced_ra ( id_id , shallowcu_forced_ra )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: shallowcu_forced_ra
  INTEGER id_id
  model_config_rec%shallowcu_forced_ra(id_id) = shallowcu_forced_ra
  RETURN
END SUBROUTINE nl_set_shallowcu_forced_ra
SUBROUTINE nl_set_numbins ( id_id , numbins )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: numbins
  INTEGER id_id
  model_config_rec%numbins(id_id) = numbins
  RETURN
END SUBROUTINE nl_set_numbins
SUBROUTINE nl_set_thbinsize ( id_id , thbinsize )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: thbinsize
  INTEGER id_id
  model_config_rec%thbinsize(id_id) = thbinsize
  RETURN
END SUBROUTINE nl_set_thbinsize
SUBROUTINE nl_set_rbinsize ( id_id , rbinsize )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: rbinsize
  INTEGER id_id
  model_config_rec%rbinsize(id_id) = rbinsize
  RETURN
END SUBROUTINE nl_set_rbinsize
SUBROUTINE nl_set_mindeepfreq ( id_id , mindeepfreq )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: mindeepfreq
  INTEGER id_id
  model_config_rec%mindeepfreq(id_id) = mindeepfreq
  RETURN
END SUBROUTINE nl_set_mindeepfreq
SUBROUTINE nl_set_minshallowfreq ( id_id , minshallowfreq )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: minshallowfreq
  INTEGER id_id
  model_config_rec%minshallowfreq(id_id) = minshallowfreq
  RETURN
END SUBROUTINE nl_set_minshallowfreq
SUBROUTINE nl_set_shcu_aerosols_opt ( id_id , shcu_aerosols_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shcu_aerosols_opt
  INTEGER id_id
  model_config_rec%shcu_aerosols_opt(id_id) = shcu_aerosols_opt
  RETURN
END SUBROUTINE nl_set_shcu_aerosols_opt
SUBROUTINE nl_set_icloud_cu ( id_id , icloud_cu )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: icloud_cu
  INTEGER id_id
  model_config_rec%icloud_cu(id_id) = icloud_cu
  RETURN
END SUBROUTINE nl_set_icloud_cu
SUBROUTINE nl_set_pxlsm_smois_init ( id_id , pxlsm_smois_init )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: pxlsm_smois_init
  INTEGER id_id
  model_config_rec%pxlsm_smois_init(id_id) = pxlsm_smois_init
  RETURN
END SUBROUTINE nl_set_pxlsm_smois_init
SUBROUTINE nl_set_pxlsm_modis_veg ( id_id , pxlsm_modis_veg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: pxlsm_modis_veg
  INTEGER id_id
  model_config_rec%pxlsm_modis_veg(id_id) = pxlsm_modis_veg
  RETURN
END SUBROUTINE nl_set_pxlsm_modis_veg
SUBROUTINE nl_set_omlcall ( id_id , omlcall )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: omlcall
  INTEGER id_id
  model_config_rec%omlcall = omlcall
  RETURN
END SUBROUTINE nl_set_omlcall
SUBROUTINE nl_set_sf_ocean_physics ( id_id , sf_ocean_physics )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: sf_ocean_physics
  INTEGER id_id
  model_config_rec%sf_ocean_physics = sf_ocean_physics
  RETURN
END SUBROUTINE nl_set_sf_ocean_physics
SUBROUTINE nl_set_traj_opt ( id_id , traj_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: traj_opt
  INTEGER id_id
  model_config_rec%traj_opt = traj_opt
  RETURN
END SUBROUTINE nl_set_traj_opt
SUBROUTINE nl_set_dm_has_traj ( id_id , dm_has_traj )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: dm_has_traj
  INTEGER id_id
  model_config_rec%dm_has_traj(id_id) = dm_has_traj
  RETURN
END SUBROUTINE nl_set_dm_has_traj
SUBROUTINE nl_set_tracercall ( id_id , tracercall )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tracercall
  INTEGER id_id
  model_config_rec%tracercall = tracercall
  RETURN
END SUBROUTINE nl_set_tracercall
SUBROUTINE nl_set_shalwater_z0 ( id_id , shalwater_z0 )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: shalwater_z0
  INTEGER id_id
  model_config_rec%shalwater_z0(id_id) = shalwater_z0
  RETURN
END SUBROUTINE nl_set_shalwater_z0
SUBROUTINE nl_set_shalwater_depth ( id_id , shalwater_depth )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: shalwater_depth
  INTEGER id_id
  model_config_rec%shalwater_depth = shalwater_depth
  RETURN
END SUBROUTINE nl_set_shalwater_depth
SUBROUTINE nl_set_omdt ( id_id , omdt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: omdt
  INTEGER id_id
  model_config_rec%omdt = omdt
  RETURN
END SUBROUTINE nl_set_omdt
SUBROUTINE nl_set_oml_hml0 ( id_id , oml_hml0 )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: oml_hml0
  INTEGER id_id
  model_config_rec%oml_hml0 = oml_hml0
  RETURN
END SUBROUTINE nl_set_oml_hml0
SUBROUTINE nl_set_oml_gamma ( id_id , oml_gamma )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: oml_gamma
  INTEGER id_id
  model_config_rec%oml_gamma = oml_gamma
  RETURN
END SUBROUTINE nl_set_oml_gamma
SUBROUTINE nl_set_oml_relaxation_time ( id_id , oml_relaxation_time )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: oml_relaxation_time
  INTEGER id_id
  model_config_rec%oml_relaxation_time = oml_relaxation_time
  RETURN
END SUBROUTINE nl_set_oml_relaxation_time
SUBROUTINE nl_set_isftcflx ( id_id , isftcflx )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: isftcflx
  INTEGER id_id
  model_config_rec%isftcflx = isftcflx
  RETURN
END SUBROUTINE nl_set_isftcflx
SUBROUTINE nl_set_iz0tlnd ( id_id , iz0tlnd )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: iz0tlnd
  INTEGER id_id
  model_config_rec%iz0tlnd = iz0tlnd
  RETURN
END SUBROUTINE nl_set_iz0tlnd
SUBROUTINE nl_set_shadlen ( id_id , shadlen )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: shadlen
  INTEGER id_id
  model_config_rec%shadlen = shadlen
  RETURN
END SUBROUTINE nl_set_shadlen
SUBROUTINE nl_set_slope_rad ( id_id , slope_rad )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: slope_rad
  INTEGER id_id
  model_config_rec%slope_rad(id_id) = slope_rad
  RETURN
END SUBROUTINE nl_set_slope_rad
SUBROUTINE nl_set_topo_shading ( id_id , topo_shading )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: topo_shading
  INTEGER id_id
  model_config_rec%topo_shading(id_id) = topo_shading
  RETURN
END SUBROUTINE nl_set_topo_shading
SUBROUTINE nl_set_topo_wind ( id_id , topo_wind )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: topo_wind
  INTEGER id_id
  model_config_rec%topo_wind(id_id) = topo_wind
  RETURN
END SUBROUTINE nl_set_topo_wind
SUBROUTINE nl_set_no_mp_heating ( id_id , no_mp_heating )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: no_mp_heating
  INTEGER id_id
  model_config_rec%no_mp_heating = no_mp_heating
  RETURN
END SUBROUTINE nl_set_no_mp_heating
SUBROUTINE nl_set_fractional_seaice ( id_id , fractional_seaice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: fractional_seaice
  INTEGER id_id
  model_config_rec%fractional_seaice = fractional_seaice
  RETURN
END SUBROUTINE nl_set_fractional_seaice
SUBROUTINE nl_set_seaice_snowdepth_opt ( id_id , seaice_snowdepth_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: seaice_snowdepth_opt
  INTEGER id_id
  model_config_rec%seaice_snowdepth_opt = seaice_snowdepth_opt
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_opt
SUBROUTINE nl_set_seaice_snowdepth_max ( id_id , seaice_snowdepth_max )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: seaice_snowdepth_max
  INTEGER id_id
  model_config_rec%seaice_snowdepth_max = seaice_snowdepth_max
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_max
SUBROUTINE nl_set_seaice_snowdepth_min ( id_id , seaice_snowdepth_min )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: seaice_snowdepth_min
  INTEGER id_id
  model_config_rec%seaice_snowdepth_min = seaice_snowdepth_min
  RETURN
END SUBROUTINE nl_set_seaice_snowdepth_min
SUBROUTINE nl_set_seaice_albedo_opt ( id_id , seaice_albedo_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: seaice_albedo_opt
  INTEGER id_id
  model_config_rec%seaice_albedo_opt = seaice_albedo_opt
  RETURN
END SUBROUTINE nl_set_seaice_albedo_opt
SUBROUTINE nl_set_seaice_albedo_default ( id_id , seaice_albedo_default )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: seaice_albedo_default
  INTEGER id_id
  model_config_rec%seaice_albedo_default = seaice_albedo_default
  RETURN
END SUBROUTINE nl_set_seaice_albedo_default
SUBROUTINE nl_set_seaice_thickness_opt ( id_id , seaice_thickness_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: seaice_thickness_opt
  INTEGER id_id
  model_config_rec%seaice_thickness_opt = seaice_thickness_opt
  RETURN
END SUBROUTINE nl_set_seaice_thickness_opt
SUBROUTINE nl_set_seaice_thickness_default ( id_id , seaice_thickness_default )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: seaice_thickness_default
  INTEGER id_id
  model_config_rec%seaice_thickness_default = seaice_thickness_default
  RETURN
END SUBROUTINE nl_set_seaice_thickness_default
SUBROUTINE nl_set_tice2tsk_if2cold ( id_id , tice2tsk_if2cold )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: tice2tsk_if2cold
  INTEGER id_id
  model_config_rec%tice2tsk_if2cold = tice2tsk_if2cold
  RETURN
END SUBROUTINE nl_set_tice2tsk_if2cold
SUBROUTINE nl_set_bucket_mm ( id_id , bucket_mm )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: bucket_mm
  INTEGER id_id
  model_config_rec%bucket_mm = bucket_mm
  RETURN
END SUBROUTINE nl_set_bucket_mm
SUBROUTINE nl_set_bucket_j ( id_id , bucket_j )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: bucket_j
  INTEGER id_id
  model_config_rec%bucket_j = bucket_j
  RETURN
END SUBROUTINE nl_set_bucket_j
SUBROUTINE nl_set_mp_tend_lim ( id_id , mp_tend_lim )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: mp_tend_lim
  INTEGER id_id
  model_config_rec%mp_tend_lim = mp_tend_lim
  RETURN
END SUBROUTINE nl_set_mp_tend_lim
SUBROUTINE nl_set_prec_acc_dt ( id_id , prec_acc_dt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: prec_acc_dt
  INTEGER id_id
  model_config_rec%prec_acc_dt(id_id) = prec_acc_dt
  RETURN
END SUBROUTINE nl_set_prec_acc_dt
SUBROUTINE nl_set_prec_acc_opt ( id_id , prec_acc_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: prec_acc_opt
  INTEGER id_id
  model_config_rec%prec_acc_opt = prec_acc_opt
  RETURN
END SUBROUTINE nl_set_prec_acc_opt
SUBROUTINE nl_set_bucketr_opt ( id_id , bucketr_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bucketr_opt
  INTEGER id_id
  model_config_rec%bucketr_opt = bucketr_opt
  RETURN
END SUBROUTINE nl_set_bucketr_opt
SUBROUTINE nl_set_bucketf_opt ( id_id , bucketf_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: bucketf_opt
  INTEGER id_id
  model_config_rec%bucketf_opt = bucketf_opt
  RETURN
END SUBROUTINE nl_set_bucketf_opt
SUBROUTINE nl_set_process_time_series ( id_id , process_time_series )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: process_time_series
  INTEGER id_id
  model_config_rec%process_time_series = process_time_series
  RETURN
END SUBROUTINE nl_set_process_time_series
SUBROUTINE nl_set_grav_settling ( id_id , grav_settling )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: grav_settling
  INTEGER id_id
  model_config_rec%grav_settling(id_id) = grav_settling
  RETURN
END SUBROUTINE nl_set_grav_settling
SUBROUTINE nl_set_sas_pgcon ( id_id , sas_pgcon )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: sas_pgcon
  INTEGER id_id
  model_config_rec%sas_pgcon(id_id) = sas_pgcon
  RETURN
END SUBROUTINE nl_set_sas_pgcon
SUBROUTINE nl_set_scalar_pblmix ( id_id , scalar_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: scalar_pblmix
  INTEGER id_id
  model_config_rec%scalar_pblmix(id_id) = scalar_pblmix
  RETURN
END SUBROUTINE nl_set_scalar_pblmix
SUBROUTINE nl_set_tracer_pblmix ( id_id , tracer_pblmix )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: tracer_pblmix
  INTEGER id_id
  model_config_rec%tracer_pblmix(id_id) = tracer_pblmix
  RETURN
END SUBROUTINE nl_set_tracer_pblmix
SUBROUTINE nl_set_use_aero_icbc ( id_id , use_aero_icbc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_aero_icbc
  INTEGER id_id
  model_config_rec%use_aero_icbc = use_aero_icbc
  RETURN
END SUBROUTINE nl_set_use_aero_icbc
SUBROUTINE nl_set_use_rap_aero_icbc ( id_id , use_rap_aero_icbc )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: use_rap_aero_icbc
  INTEGER id_id
  model_config_rec%use_rap_aero_icbc = use_rap_aero_icbc
  RETURN
END SUBROUTINE nl_set_use_rap_aero_icbc
SUBROUTINE nl_set_aer_init_opt ( id_id , aer_init_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_init_opt
  INTEGER id_id
  model_config_rec%aer_init_opt = aer_init_opt
  RETURN
END SUBROUTINE nl_set_aer_init_opt
SUBROUTINE nl_set_wif_fire_emit ( id_id , wif_fire_emit )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: wif_fire_emit
  INTEGER id_id
  model_config_rec%wif_fire_emit = wif_fire_emit
  RETURN
END SUBROUTINE nl_set_wif_fire_emit
SUBROUTINE nl_set_aer_fire_emit_opt ( id_id , aer_fire_emit_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: aer_fire_emit_opt
  INTEGER id_id
  model_config_rec%aer_fire_emit_opt = aer_fire_emit_opt
  RETURN
END SUBROUTINE nl_set_aer_fire_emit_opt
SUBROUTINE nl_set_wif_fire_inj ( id_id , wif_fire_inj )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: wif_fire_inj
  INTEGER id_id
  model_config_rec%wif_fire_inj(id_id) = wif_fire_inj
  RETURN
END SUBROUTINE nl_set_wif_fire_inj
SUBROUTINE nl_set_use_mp_re ( id_id , use_mp_re )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: use_mp_re
  INTEGER id_id
  model_config_rec%use_mp_re = use_mp_re
  RETURN
END SUBROUTINE nl_set_use_mp_re
SUBROUTINE nl_set_insert_init_cloud ( id_id , insert_init_cloud )
  USE module_configure, ONLY : model_config_rec
  logical , INTENT(IN) :: insert_init_cloud
  INTEGER id_id
  model_config_rec%insert_init_cloud = insert_init_cloud
  RETURN
END SUBROUTINE nl_set_insert_init_cloud
SUBROUTINE nl_set_ccn_conc ( id_id , ccn_conc )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: ccn_conc
  INTEGER id_id
  model_config_rec%ccn_conc = ccn_conc
  RETURN
END SUBROUTINE nl_set_ccn_conc
SUBROUTINE nl_set_hail_opt ( id_id , hail_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: hail_opt
  INTEGER id_id
  model_config_rec%hail_opt = hail_opt
  RETURN
END SUBROUTINE nl_set_hail_opt
SUBROUTINE nl_set_morr_rimed_ice ( id_id , morr_rimed_ice )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: morr_rimed_ice
  INTEGER id_id
  model_config_rec%morr_rimed_ice = morr_rimed_ice
  RETURN
END SUBROUTINE nl_set_morr_rimed_ice
SUBROUTINE nl_set_clean_atm_diag ( id_id , clean_atm_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: clean_atm_diag
  INTEGER id_id
  model_config_rec%clean_atm_diag = clean_atm_diag
  RETURN
END SUBROUTINE nl_set_clean_atm_diag
SUBROUTINE nl_set_calc_clean_atm_diag ( id_id , calc_clean_atm_diag )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: calc_clean_atm_diag
  INTEGER id_id
  model_config_rec%calc_clean_atm_diag = calc_clean_atm_diag
  RETURN
END SUBROUTINE nl_set_calc_clean_atm_diag
SUBROUTINE nl_set_acc_phy_tend ( id_id , acc_phy_tend )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: acc_phy_tend
  INTEGER id_id
  model_config_rec%acc_phy_tend(id_id) = acc_phy_tend
  RETURN
END SUBROUTINE nl_set_acc_phy_tend
SUBROUTINE nl_set_madwrf_opt ( id_id , madwrf_opt )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: madwrf_opt
  INTEGER id_id
  model_config_rec%madwrf_opt = madwrf_opt
  RETURN
END SUBROUTINE nl_set_madwrf_opt
SUBROUTINE nl_set_madwrf_dt_relax ( id_id , madwrf_dt_relax )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: madwrf_dt_relax
  INTEGER id_id
  model_config_rec%madwrf_dt_relax = madwrf_dt_relax
  RETURN
END SUBROUTINE nl_set_madwrf_dt_relax
SUBROUTINE nl_set_madwrf_dt_nudge ( id_id , madwrf_dt_nudge )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: madwrf_dt_nudge
  INTEGER id_id
  model_config_rec%madwrf_dt_nudge = madwrf_dt_nudge
  RETURN
END SUBROUTINE nl_set_madwrf_dt_nudge
SUBROUTINE nl_set_madwrf_cldinit ( id_id , madwrf_cldinit )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: madwrf_cldinit
  INTEGER id_id
  model_config_rec%madwrf_cldinit = madwrf_cldinit
  RETURN
END SUBROUTINE nl_set_madwrf_cldinit
SUBROUTINE nl_set_dveg ( id_id , dveg )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: dveg
  INTEGER id_id
  model_config_rec%dveg = dveg
  RETURN
END SUBROUTINE nl_set_dveg
SUBROUTINE nl_set_opt_crs ( id_id , opt_crs )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_crs
  INTEGER id_id
  model_config_rec%opt_crs = opt_crs
  RETURN
END SUBROUTINE nl_set_opt_crs
SUBROUTINE nl_set_opt_btr ( id_id , opt_btr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_btr
  INTEGER id_id
  model_config_rec%opt_btr = opt_btr
  RETURN
END SUBROUTINE nl_set_opt_btr
SUBROUTINE nl_set_opt_run ( id_id , opt_run )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_run
  INTEGER id_id
  model_config_rec%opt_run = opt_run
  RETURN
END SUBROUTINE nl_set_opt_run
SUBROUTINE nl_set_opt_sfc ( id_id , opt_sfc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_sfc
  INTEGER id_id
  model_config_rec%opt_sfc = opt_sfc
  RETURN
END SUBROUTINE nl_set_opt_sfc
SUBROUTINE nl_set_opt_frz ( id_id , opt_frz )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_frz
  INTEGER id_id
  model_config_rec%opt_frz = opt_frz
  RETURN
END SUBROUTINE nl_set_opt_frz
SUBROUTINE nl_set_opt_inf ( id_id , opt_inf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_inf
  INTEGER id_id
  model_config_rec%opt_inf = opt_inf
  RETURN
END SUBROUTINE nl_set_opt_inf
SUBROUTINE nl_set_opt_rad ( id_id , opt_rad )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_rad
  INTEGER id_id
  model_config_rec%opt_rad = opt_rad
  RETURN
END SUBROUTINE nl_set_opt_rad
SUBROUTINE nl_set_opt_alb ( id_id , opt_alb )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_alb
  INTEGER id_id
  model_config_rec%opt_alb = opt_alb
  RETURN
END SUBROUTINE nl_set_opt_alb
SUBROUTINE nl_set_opt_snf ( id_id , opt_snf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_snf
  INTEGER id_id
  model_config_rec%opt_snf = opt_snf
  RETURN
END SUBROUTINE nl_set_opt_snf
SUBROUTINE nl_set_opt_tbot ( id_id , opt_tbot )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_tbot
  INTEGER id_id
  model_config_rec%opt_tbot = opt_tbot
  RETURN
END SUBROUTINE nl_set_opt_tbot
SUBROUTINE nl_set_opt_stc ( id_id , opt_stc )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_stc
  INTEGER id_id
  model_config_rec%opt_stc = opt_stc
  RETURN
END SUBROUTINE nl_set_opt_stc
SUBROUTINE nl_set_opt_gla ( id_id , opt_gla )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_gla
  INTEGER id_id
  model_config_rec%opt_gla = opt_gla
  RETURN
END SUBROUTINE nl_set_opt_gla
SUBROUTINE nl_set_opt_rsf ( id_id , opt_rsf )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_rsf
  INTEGER id_id
  model_config_rec%opt_rsf = opt_rsf
  RETURN
END SUBROUTINE nl_set_opt_rsf
SUBROUTINE nl_set_opt_soil ( id_id , opt_soil )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_soil
  INTEGER id_id
  model_config_rec%opt_soil = opt_soil
  RETURN
END SUBROUTINE nl_set_opt_soil
SUBROUTINE nl_set_opt_pedo ( id_id , opt_pedo )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_pedo
  INTEGER id_id
  model_config_rec%opt_pedo = opt_pedo
  RETURN
END SUBROUTINE nl_set_opt_pedo
SUBROUTINE nl_set_opt_crop ( id_id , opt_crop )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_crop
  INTEGER id_id
  model_config_rec%opt_crop = opt_crop
  RETURN
END SUBROUTINE nl_set_opt_crop
SUBROUTINE nl_set_opt_irr ( id_id , opt_irr )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_irr
  INTEGER id_id
  model_config_rec%opt_irr = opt_irr
  RETURN
END SUBROUTINE nl_set_opt_irr
SUBROUTINE nl_set_opt_irrm ( id_id , opt_irrm )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_irrm
  INTEGER id_id
  model_config_rec%opt_irrm = opt_irrm
  RETURN
END SUBROUTINE nl_set_opt_irrm
SUBROUTINE nl_set_opt_infdv ( id_id , opt_infdv )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_infdv
  INTEGER id_id
  model_config_rec%opt_infdv = opt_infdv
  RETURN
END SUBROUTINE nl_set_opt_infdv
SUBROUTINE nl_set_opt_tdrn ( id_id , opt_tdrn )
  USE module_configure, ONLY : model_config_rec
  integer , INTENT(IN) :: opt_tdrn
  INTEGER id_id
  model_config_rec%opt_tdrn = opt_tdrn
  RETURN
END SUBROUTINE nl_set_opt_tdrn
SUBROUTINE nl_set_soiltstep ( id_id , soiltstep )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: soiltstep
  INTEGER id_id
  model_config_rec%soiltstep = soiltstep
  RETURN
END SUBROUTINE nl_set_soiltstep
SUBROUTINE nl_set_wtddt ( id_id , wtddt )
  USE module_configure, ONLY : model_config_rec
  real , INTENT(IN) :: wtddt
  INTEGER id_id
  model_config_rec%wtddt(id_id) = wtddt
  RETURN
END SUBROUTINE nl_set_wtddt
!ENDOFREGISTRYGENERATEDINCLUDE
