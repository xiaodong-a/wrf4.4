MODULE module_chem_share
USE module_state_description
CONTAINS


     integer FUNCTION get_last_gas(chem_opt)
     implicit none
     integer, intent(in) :: chem_opt

     
     

     select case (chem_opt)
     case (0)
        get_last_gas = 0


     case (RADM2, RADM2_KPP, RADM2SORG, RADM2SORG_AQ, RADM2SORG_AQCHEM, RADM2SORG_KPP, &
           RACM_KPP, RACMPM_KPP, RACM_MIM_KPP, RACMSORG_AQ, RACMSORG_AQCHEM_KPP,       &
           RACM_ESRLSORG_AQCHEM_KPP, RACM_ESRLSORG_KPP, RACMSORG_KPP, RACM_SOA_VBS_KPP,&
           RACM_SOA_VBS_AQCHEM_KPP,GOCARTRACM_KPP,GOCARTRADM2,  &
           RACM_SOA_VBS_HET_KPP)
        get_last_gas = p_ho2

     case (SAPRC99_KPP,SAPRC99_MOSAIC_4BIN_VBS2_KPP, &
          SAPRC99_MOSAIC_8BIN_VBS2_AQ_KPP,SAPRC99_MOSAIC_8BIN_VBS2_KPP)
        get_last_gas = p_ch4

     case (CBMZ,CBMZ_MOSAIC_DMS_4BIN,CBMZ_MOSAIC_DMS_8BIN,CBMZ_MOSAIC_DMS_4BIN_AQ,CBMZ_MOSAIC_DMS_8BIN_AQ)
        get_last_gas = p_mtf

     case (CBMZ_BB,CBMZ_BB_KPP, CBMZ_MOSAIC_KPP, CBMZ_MOSAIC_4BIN, &
           CBMZ_MOSAIC_8BIN,CBMZ_MOSAIC_4BIN_AQ,CBMZ_MOSAIC_8BIN_AQ,CBMZSORG,CBMZSORG_AQ)
        get_last_gas = p_isopo2
     case (CHEM_TRACER)
        get_last_gas = p_co

     case (CHEM_TRACE2)
        get_last_gas = p_tracer_1

     case (GOCART_SIMPLE)
        get_last_gas = p_msa

     case (CBM4_KPP)
        get_last_gas = p_ho2

     case (CB05_SORG_AQ_KPP, CB05_SORG_VBS_AQ_KPP)
        get_last_gas = p_nh3

     case (CHEM_VASH)
        get_last_gas = 0
     case (CHEM_VOLC)
        get_last_gas = p_sulf
     case (CHEM_VOLC_4BIN)
        get_last_gas = 0
     case (DUST)
        get_last_gas = 0
     case (MOZART_KPP)
        get_last_gas = p_meko2

     case (CRIMECH_KPP, CRI_MOSAIC_8BIN_AQ_KPP, CRI_MOSAIC_4BIN_AQ_KPP)
        GET_LAST_GAS = p_ic3h7no3

     case (MOZCART_KPP)
        get_last_gas = p_meko2

     case (T1_MOZCART_KPP)
        get_last_gas = p_xylolo2

     case (MOZART_MOSAIC_4BIN_KPP)
        get_last_gas = p_meko2

     case (MOZART_MOSAIC_4BIN_AQ_KPP)
        get_last_gas = p_meko2

     case (CO2_TRACER,GHG_TRACER) 
        get_last_gas = 0
     case ( CBMZ_CAM_MAM3_NOAQ, CBMZ_CAM_MAM3_AQ, CBMZ_CAM_MAM7_NOAQ, CBMZ_CAM_MAM7_AQ )
        get_last_gas = p_soag
     case default
        call wrf_error_fatal3("<stdin>",81,&
"get_last_gas: could not decipher chem_opt value")
     end select

   END FUNCTION get_last_gas

END MODULE module_chem_share
