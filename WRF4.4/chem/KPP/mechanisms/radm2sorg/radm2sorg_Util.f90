! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Auxiliary Routines File
! 
! Generated by KPP-2.1 symbolic chemistry Kinetics PreProcessor
!       (http://www.cs.vt.edu/~asandu/Software/KPP)
! KPP is distributed under GPL, the general public licence
!       (http://www.gnu.org/copyleft/gpl.html)
! (C) 1995-1997, V. Damian & A. Sandu, CGRER, Univ. Iowa
! (C) 1997-2005, A. Sandu, Michigan Tech, Virginia Tech
!     With important contributions from:
!        M. Damian, Villanova University, USA
!        R. Sander, Max-Planck Institute for Chemistry, Mainz, Germany
! 
! File                 : radm2sorg_Util.f90
! Time                 : Mon Sep 26 23:37:47 2022
! Working directory    : /home/ess/xiaodong/WORK/WRFCHEM/model/WRF4.4/chem/KPP/mechanisms/radm2sorg
! Equation file        : radm2sorg.kpp
! Output root filename : radm2sorg
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



MODULE radm2sorg_Util

  USE radm2sorg_Parameters
  IMPLICIT NONE

CONTAINS



! User INLINED Utility Functions

! End INLINED Utility Functions

! Utility Functions from KPP_HOME/util/util
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! UTIL - Utility functions
!   Arguments :
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

! ****************************************************************
!                            
! InitSaveData - Opens the data file for writing
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE InitSaveData ()

      USE radm2sorg_Parameters

      open(10, file='radm2sorg.dat')

      END SUBROUTINE InitSaveData

! End of InitSaveData function
! ****************************************************************

! ****************************************************************
!                            
! SaveData - Write LOOKAT species in the data file 
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE SaveData ()

      USE radm2sorg_Global
      USE radm2sorg_Monitor

      INTEGER i

      WRITE(10,999) (TIME-TSTART)/3600.D0,  &
                   (C(LOOKAT(i))/CFACTOR, i=1,NLOOKAT)
999   FORMAT(E24.16,100(1X,E24.16))

      END SUBROUTINE SaveData

! End of SaveData function
! ****************************************************************

! ****************************************************************
!                            
! CloseSaveData - Close the data file 
!   Parameters :                                                  
!
! ****************************************************************

      SUBROUTINE CloseSaveData ()

      USE radm2sorg_Parameters

      CLOSE(10)

      END SUBROUTINE CloseSaveData

! End of CloseSaveData function
! ****************************************************************

! ****************************************************************
!                            
! GenerateMatlab - Generates MATLAB file to load the data file 
!   Parameters : 
!                It will have a character string to prefix each 
!                species name with.                                                 
!
! ****************************************************************

      SUBROUTINE GenerateMatlab ( PREFIX )

      USE radm2sorg_Parameters
      USE radm2sorg_Global
      USE radm2sorg_Monitor

      
      CHARACTER(LEN=8) PREFIX 
      INTEGER i

      open(20, file='radm2sorg.m')
      write(20,*) 'load radm2sorg.dat;'
      write(20,990) PREFIX
990   FORMAT(A1,'c = radm2sorg;')
      write(20,*) 'clear radm2sorg;'
      write(20,991) PREFIX, PREFIX
991   FORMAT(A1,'t=',A1,'c(:,1);')
      write(20,992) PREFIX
992   FORMAT(A1,'c(:,1)=[];')

      do i=1,NLOOKAT
        write(20,993) PREFIX, SPC_NAMES(LOOKAT(i)), PREFIX, i
993     FORMAT(A1,A6,' = ',A1,'c(:,',I2,');')
      end do
      
      CLOSE(20)

      END SUBROUTINE GenerateMatlab

! End of GenerateMatlab function
! ****************************************************************


! End Utility Functions from KPP_HOME/util/util
! End of UTIL function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Shuffle_user2kpp - function to copy concentrations from USER to KPP
!   Arguments :
!      V_USER    - Concentration of variable species in USER's order
!      V         - Concentrations of variable species (local)
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Shuffle_user2kpp ( V_USER, V )

! V_USER - Concentration of variable species in USER's order
  REAL(kind=dp) :: V_USER(NVAR)
! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)

  V(46) = V_USER(1)
  V(14) = V_USER(2)
  V(58) = V_USER(3)
  V(59) = V_USER(4)
  V(54) = V_USER(5)
  V(15) = V_USER(6)
  V(13) = V_USER(7)
  V(25) = V_USER(8)
  V(20) = V_USER(9)
  V(5) = V_USER(10)
  V(1) = V_USER(11)
  V(26) = V_USER(12)
  V(6) = V_USER(13)
  V(16) = V_USER(14)
  V(8) = V_USER(15)
  V(9) = V_USER(16)
  V(24) = V_USER(17)
  V(28) = V_USER(18)
  V(29) = V_USER(19)
  V(27) = V_USER(20)
  V(10) = V_USER(21)
  V(11) = V_USER(22)
  V(22) = V_USER(23)
  V(43) = V_USER(24)
  V(48) = V_USER(25)
  V(57) = V_USER(26)
  V(33) = V_USER(27)
  V(31) = V_USER(28)
  V(34) = V_USER(29)
  V(52) = V_USER(30)
  V(30) = V_USER(31)
  V(47) = V_USER(32)
  V(23) = V_USER(33)
  V(12) = V_USER(34)
  V(21) = V_USER(35)
  V(42) = V_USER(36)
  V(18) = V_USER(37)
  V(2) = V_USER(38)
  V(3) = V_USER(39)
  V(53) = V_USER(40)
  V(51) = V_USER(41)
  V(19) = V_USER(42)
  V(7) = V_USER(43)
  V(56) = V_USER(44)
  V(41) = V_USER(45)
  V(44) = V_USER(46)
  V(37) = V_USER(47)
  V(49) = V_USER(48)
  V(35) = V_USER(49)
  V(36) = V_USER(50)
  V(55) = V_USER(51)
  V(45) = V_USER(52)
  V(50) = V_USER(53)
  V(38) = V_USER(54)
  V(39) = V_USER(55)
  V(32) = V_USER(56)
  V(17) = V_USER(57)
  V(4) = V_USER(58)
  V(40) = V_USER(59)
      
END SUBROUTINE Shuffle_user2kpp

! End of Shuffle_user2kpp function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! Shuffle_kpp2user - function to restore concentrations from KPP to USER
!   Arguments :
!      V         - Concentrations of variable species (local)
!      V_USER    - Concentration of variable species in USER's order
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE Shuffle_kpp2user ( V, V_USER )

! V - Concentrations of variable species (local)
  REAL(kind=dp) :: V(NVAR)
! V_USER - Concentration of variable species in USER's order
  REAL(kind=dp) :: V_USER(NVAR)

  V_USER(1) = V(46)
  V_USER(2) = V(14)
  V_USER(3) = V(58)
  V_USER(4) = V(59)
  V_USER(5) = V(54)
  V_USER(6) = V(15)
  V_USER(7) = V(13)
  V_USER(8) = V(25)
  V_USER(9) = V(20)
  V_USER(10) = V(5)
  V_USER(11) = V(1)
  V_USER(12) = V(26)
  V_USER(13) = V(6)
  V_USER(14) = V(16)
  V_USER(15) = V(8)
  V_USER(16) = V(9)
  V_USER(17) = V(24)
  V_USER(18) = V(28)
  V_USER(19) = V(29)
  V_USER(20) = V(27)
  V_USER(21) = V(10)
  V_USER(22) = V(11)
  V_USER(23) = V(22)
  V_USER(24) = V(43)
  V_USER(25) = V(48)
  V_USER(26) = V(57)
  V_USER(27) = V(33)
  V_USER(28) = V(31)
  V_USER(29) = V(34)
  V_USER(30) = V(52)
  V_USER(31) = V(30)
  V_USER(32) = V(47)
  V_USER(33) = V(23)
  V_USER(34) = V(12)
  V_USER(35) = V(21)
  V_USER(36) = V(42)
  V_USER(37) = V(18)
  V_USER(38) = V(2)
  V_USER(39) = V(3)
  V_USER(40) = V(53)
  V_USER(41) = V(51)
  V_USER(42) = V(19)
  V_USER(43) = V(7)
  V_USER(44) = V(56)
  V_USER(45) = V(41)
  V_USER(46) = V(44)
  V_USER(47) = V(37)
  V_USER(48) = V(49)
  V_USER(49) = V(35)
  V_USER(50) = V(36)
  V_USER(51) = V(55)
  V_USER(52) = V(45)
  V_USER(53) = V(50)
  V_USER(54) = V(38)
  V_USER(55) = V(39)
  V_USER(56) = V(32)
  V_USER(57) = V(17)
  V_USER(58) = V(4)
  V_USER(59) = V(40)
      
END SUBROUTINE Shuffle_kpp2user

! End of Shuffle_kpp2user function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
! 
! GetMass - compute total mass of selected atoms
!   Arguments :
!      CL        - Concentration of all species (local)
!      Mass      - value of mass balance
! 
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

SUBROUTINE GetMass ( CL, Mass )

! CL - Concentration of all species (local)
  REAL(kind=dp) :: CL(NSPEC)
! Mass - value of mass balance
  REAL(kind=dp) :: Mass(1)

      
END SUBROUTINE GetMass

! End of GetMass function
! ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



END MODULE radm2sorg_Util

