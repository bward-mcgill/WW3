#include "w3macros.h"
!/ ------------------------------------------------------------------- /
      MODULE W3REF1MD
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III                     |
!/                  |            F. Ardhuin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         27-Jun-2014 |
!/                  +-----------------------------------+
!/
!/    31-Mar-2010 : Origination.                        ( version 3.14.IFREMER )
!/    03-Sep-2010 : Clean up                            ( version 3.14.IFREMER )
!/    31-May-2011 : Adding variable reflections         ( version 4.01 )
!/    02-Nov-2011 : Compatibility with unst. grids      ( version 4.04 )
!/    24-Fev-2012 : Correction of angle in fluxes       ( version 4.05 )
!/    27-Jul-2013 : Adding free infragravity waves      ( version 4.11 )
!/    11-Nov-2013 : Extends IG energy into main band    ( version 4.13 )
!/    11-Jun-2014 : Put reflection by subgrids back     ( version 5.01 )
!/    27-Jun-2014 : Modifies subgrid reflection of IG   ( version 5.01 )
!/
!  1. Purpose :
!
!     This module computes :
!        - shoreline reflection
!        - unresolved islands and iceberg reflections
!
!  2. Variables and types :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!     ----------------------------------------------------------------
!
!  3. Subroutines and functions :
!
!      Name      Type  Scope    Description
!     ----------------------------------------------------------------
!      W3SREF    Subr. Public   Reflection of waves (shorline, islands...)
!     ----------------------------------------------------------------
!
!  4. Subroutines and functions used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Remarks :
!
!  6. Switches :
!
!     !/S  Enable subroutine tracing.
!
!  7. Source code :
!/
!/ ------------------------------------------------------------------- /
!/
!
      PUBLIC
!/
!/ Public variables
!/
!
!/
      CONTAINS
!/ ------------------------------------------------------------------- /
      SUBROUTINE W3SREF(A, CG, WN, EMEAN, FMEAN, DEPTH, CX1, CY1, REFLC, REFLD,     &
                         TRNX, TRNY, BERG, DT,  IX, IY,S)
!/
!/                  +-----------------------------------+
!/                  | WAVEWATCH III           NOAA/NCEP |
!/                  |            F. Ardhuin             |
!/                  |                        FORTRAN 90 |
!/                  | Last update :         11-Jun-2014 |
!/                  +-----------------------------------+
!/
!/    06-May-2010 : Origination.                          ( version 3.14-Ifremer )
!/    31-May-2011 : Introduction of amplitude-dependent R ( version 4.05 )
!/    27-Jul-2013 : Adding free infragravity waves        ( version 4.11 )
!/    11-Nov-2013 : Expands IG energy frequency range     ( version 4.13 )
!/    05-Mar-2014 : Fixing bug with ICALC = 1 and IG1     ( version 4.18 )
!/    11-Jun-2014 : Put reflection by subgrids back       ( version 5.01 )
!/
!  1. Purpose :
!
!     Computes coastal and iceberg/island reflections and adds free IG energy
!
!  2. Method :
!
!     Adds the reflected components from 2 types of sources:
!        shoreline reflection, subgrid obstruction and icebergs
!
!     In the case where the IG switch is present, there are two passes:
!        - ICALC = 1, only the wind sea and swell are reflected (no IG added)
!        - ICALC = 2, IG energy is added into all frequency bands
!
!     When IG energy is put in the entire spectrum ( NINT(IGPARS(4)).EQ.2 )
!        two passes are done: the first for the reflection of windsea and swell
!                             the second for the addition of IG and IG reflection alone
!
!  3. Parameters :
!
!     Parameter list
!     ----------------------------------------------------------------
!  A         R.A.  I   Action density spectrum (1-D)
!       CG        R.A.  I   Group velocities.
!       WN        R.A.  I   Wavenumbers.
!       DEPTH     Real  I   Mean water depth.
!       S         R.A.  O   Source term (1-D version).
!       D         R.A.  O   Diagonal term of derivative (1-D version).
!     ----------------------------------------------------------------
!
!  4. Subroutines used :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      STRACE    Subr. W3SERVMD Subroutine tracing.
!     ----------------------------------------------------------------
!
!  5. Called by :
!
!      Name      Type  Module   Description
!     ----------------------------------------------------------------
!      W3SRCE    Subr. W3SRCEMD Source term integration.
!      W3EXPO    Subr.   N/A    Point output post-processor.
!      GXEXPO    Subr.   N/A    GrADS point output post-processor.
!     ----------------------------------------------------------------
!
!  6. Error messages :
!
!       None.
!
!  7. Remarks :
!
!  8. Structure :
!
!     See source code.
!
!  9. Switches :
!
!     !/S  Enable subroutine tracing.
!
! 10. Source code :
!
!/ ------------------------------------------------------------------- /
      USE CONSTANTS
      USE W3GDATMD, ONLY: NK, NTH, NSPEC, SIG, DTH, DDEN,  &
                          REFPARS, ECOS, ESIN, EC2, MAPTH, MAPWN, FLAGLL, &
                          SIG2, DSII, IOBPD, GTYPE, UNGTYPE, MAPFS, CLGTYPE, RLGTYPE
      USE W3GDATMD, ONLY : XYB, CLATS, HPFAC, HQFAC, SX, SY, SI
!/
!
      IMPLICIT NONE
!/
!/ ------------------------------------------------------------------- /
!/ Parameter list
!/
      REAL, INTENT(IN)        :: CG(NK), WN(NK), DEPTH, EMEAN, FMEAN
      REAL, INTENT(INOUT)     :: A(NSPEC)
      REAL, INTENT(IN)        :: CX1, CY1, DT
      INTEGER, INTENT(IN)     :: REFLD(6), IX, IY
      REAL, INTENT(IN)        :: REFLC(4), TRNX, &
                                 TRNY, BERG
      REAL, INTENT(OUT)       :: S(NSPEC)
!/
!/ ------------------------------------------------------------------- /
!/ Local parameters
!/
      INTEGER         :: ISPECI, ISPEC, IK, ITH, ITH2, ITH3, ITH2X, ITH2Y, &
                         NRS, IK1
      INTEGER         :: ISEA, ICALC
      LOGICAL         :: IGBCOVERWRITE, IGSWELLMAX
      REAL            :: R1, R2, R3, R4, R2X, R2Y, DEPTHIG
      REAL            :: DELA, DELX, DELY, FACX
      REAL            :: FAC1, FAC2, FAC3, FAC4, RAMP0, RAMP, &
                         RAMP1, RAMP2, RAMP4, MICHEFAC, SLOPE
     REAL             :: HS, HIG, HIG1, HIG2, EB, SB, EMEANA, FMEAN2,   &
                         FMEANA, FREQIG, EFIG, EFIG1, SQRTH, SMEANA
!/
!/ ------------------------------------------------------------------- /
!/
!
! 0.  Initializations ------------------------------------------------ *
!
! This following line is a quick fix before the bug is understood ....
      EMEANA  = 0.
      FMEANA  = 0.
      FMEAN2  = 0.
 
      DELX=1.
      DELY=1.
      ! set FACx for all grid types
      IF (FLAGLL) THEN
        FACX   =  1./(DERA * RADIUS)
      ELSE
        FACX   =  1.
      END IF
 
      ISEA = MAPFS (IY,IX)
      IF (GTYPE.EQ.RLGTYPE) THEN
        DELX=SX*CLATS(ISEA)/FACX
        DELY=SY/FACX
        END IF
      IF (GTYPE.EQ.CLGTYPE) THEN
      ! Maybe what follows works also for RLGTYPE ... to be verified
        DELX=HPFAC(IY,IX)/ FACX
        DELY=HQFAC(IY,IX)/ FACX
        END IF
 
      IF (GTYPE.EQ.UNGTYPE) THEN
        DELX=5.*SQRT(SI(IX))*(DERA * RADIUS)    ! first approximation ...
        DELY=5.*SQRT(SI(IX))*(DERA * RADIUS)    ! first approximation ...
        END IF
 
      IK1=1
      DO IK=IK1, NK
        EB  = 0.
        DO ITH=1, NTH
          EB = EB + A(ITH+(IK-1)*NTH)
          END DO
        EB   = EB * DDEN(IK) / CG(IK)
        EMEANA    = EMEANA  + EB
        FMEAN2    = FMEAN2  + EB /SIG(IK)**2
        FMEANA    = FMEANA  + EB /SIG(IK)
        END DO
      FMEANA  = TPIINV * (EMEANA / MAX ( 1.E-7 , FMEANA ))
      FMEAN2  = TPIINV * SQRT(EMEANA / MAX ( 1.E-7 , FMEAN2 ))
      FMEANA  = MAX(FMEANA,SIG(1))
!
! 1.  Sets reflection term to zero
!
      ICALC=1
      HS=4.*SQRT(EMEANA)
      S = 0.
!
      NRS=NINT(REFPARS(8))
      IF (REFPARS(6).GT.0) THEN
!
! This is the Miche parameter for a beach slope of REFLC(3)
!
        IF(REFLC(3)/=REFLC(3)) THEN ! isnan test
          SLOPE=0.001
        ELSE
          SLOPE=MAX(0.001,REFLC(3))
        END IF
        MICHEFAC=0.0001*GRAV**2*(SLOPE**5)  &
                   /(MAX(EMEANA,1E-4)*MAX(FMEANA,0.001)**4)
        RAMP0=MAX(0.07*(ALOG10(MICHEFAC)+4.5)+1.5*MICHEFAC,0.005)   ! IF REFLC(1)=1, 0.07 should be 0.007
! NB: these constants are adjusted for REFLC(1) = 0.1. If  REFLC(1)=1, 0.07 should be 0.007
      ELSE
        RAMP0=1.
        ENDIF
 
!
! 2.  Shoreline reflection =============================================== *
!
      IF (REFLC(1).GT.0) THEN
        FAC1=1/(0.5*REAL(NTH))
        FAC2=1.57/(0.5*REAL(NTH))
!           FAC3=2.6/(0.5*REAL(NTH))  ! this is for NRS=4
        FAC3=2./SUM(ABS(ECOS(1:NTH))**NRS)
        FAC4=1.
!
        DO IK=1, NK
!
! Includes frequency dependence (see Elgar et al. JPO 1994)
!
          IF (REFPARS(6).GT.0) THEN
            RAMP=(MAX((0.75*TPI*FMEANA/SIG(IK)),1.)**REFPARS(10))*RAMP0
            RAMP1=MIN(REFPARS(9),REFLC(1)*RAMP)
            RAMP2=MIN(REFPARS(9),REFLC(2)*RAMP)
          ELSE
            RAMP1=RAMP0*REFLC(1)
            RAMP2=RAMP0*REFLC(2)
            END IF
!
! Special treatment for unstructured grids when not using source term
!
          IF (GTYPE.EQ.UNGTYPE.AND.REFPARS(3).LT.0.5) THEN
            DO ITH=1, NTH
              ISPECI=ITH+(IK-1)*NTH
              A(ISPECI)=A(ISPECI)*IOBPD(ITH,IX) !puts to zero the energy not going to coast
              END DO
!
            DO ITH=1, NTH
              R1=ECOS(1+MOD(ABS(ITH-REFLD(1)),NTH))
              R1=IOBPD(ITH,IX)
              ISPECI=ITH+(IK-1)*NTH
              R2=RAMP1*A(ISPECI)
              IF (R1.GT.0.AND.R2.GT.0) THEN
!
! Determines direction of specular reflection: th3=pi+2*n-th1
!
                ITH3=1+MOD(NTH/2+NTH+2*REFLD(1)-ITH-1,NTH)
                DO ITH2=1,NTH
!
!  Adds energy into reflected directions (ITH2)
!
                  ISPEC=ITH2+(IK-1)*NTH
                  R3=ECOS(1+MOD(ABS(ITH2-REFLD(1)),NTH))
                  IF (R3.LT.0) THEN
                    R4=ECOS(1+MOD(ABS(ITH2-ITH3),NTH))*(1-IOBPD(ITH2,IX))
                    IF (R4.GT.0.) THEN
!
! Tests the type of shoreline geometry
!
                    SELECT CASE (REFLD(2))
                      CASE (0)
                   ! Sharp corner: broad reflection
                        S(ISPEC)=S(ISPEC)+R2*FAC1/DT
 
! FA: analog to following lines to be swapped in if reflection method changed
                        ! RECT CASE:
                        !  S(ISPEC)=S(ISPEC)+    &
                        !    REAL(REFLD(3))*R2*CG(IK)*ABS(ECOS(ITH2X)/DELX)*FAC1   &
                        !   +REAL(REFLD(4))*R2*CG(IK)*ABS(ESIN(ITH2Y)/DELY)*FAC1
 
 
                      CASE (1)
                   ! mild corner: average reflection
                        S(ISPEC)=S(ISPEC)+R2*ABS(R4)*FAC2/DT
                      CASE (2)
                   ! straight coast: narrow reflection
                   !     IF(ITH3.EQ.ITH2) S(ISPEC)=S(ISPEC)+R2/DT  ! THIS IS FOR SPECULAR REF.
                         S(ISPEC)=S(ISPEC)+R2*(R4**NRS) *FAC3/DT
                      END SELECT
                    END IF  ! (R4.GT.0.)
                  END IF  ! (R3.LT.0)
                END DO ! ITH2=1,NTH
              END IF  ! (R1.GT.0.AND.R2.GT.0)
            END DO  ! ITH=1, NTH
          ELSE ! (GTYPE.NE.UNGTYPE)
!
! This is for structured grids ....
!
!  Loop on  incident wave direction (ITH)
!
            DO ITH=1, NTH
              R1=ECOS(1+MOD(ABS(ITH-REFLD(1)),NTH))
              R2=RAMP1*A(ITH+(IK-1)*NTH)
              IF (R1.GT.0.AND.R2.GT.0) THEN
                DO ITH2=1,NTH
!
!  Adds energy into reflected directions (ITH2)
!
                  ISPEC=ITH2+(IK-1)*NTH
                  ITH2X=1+MOD(NTH+ITH2-REFLD(5)-1,NTH)
                  ITH2Y=1+MOD(NTH+ITH2-REFLD(6)-1,NTH)
                  R3=ECOS(1+MOD(ABS(ITH2-REFLD(1)),NTH))
                  IF (R3.LT.0) THEN
!
! Determines direction of specular reflection: th3=pi+2*n-th1
!
                    ITH3=1+MOD(NTH/2+NTH+2*REFLD(1)-ITH-1,NTH)
                    R4=ECOS(1+MOD(ABS(ITH2-ITH3),NTH))
                    IF (R4.GT.0.) THEN
!
! Tests the type of shoreline geometry
! NB: REFLD(3) or REFLD(4) is equal to 1 if the reflection is applied (real land neighbor)
                      SELECT CASE (REFLD(2))
                      CASE (0)
                   ! Sharp corner: broad reflection
                          S(ISPEC)=S(ISPEC)+    &
                            REAL(REFLD(3))*R2*CG(IK)*ABS(ECOS(ITH2X)/DELX)*FAC1   &
                           +REAL(REFLD(4))*R2*CG(IK)*ABS(ESIN(ITH2Y)/DELY)*FAC1
                      CASE (1)
                   ! mild corner: average reflection
!
                          S(ISPEC)=S(ISPEC)+    &
                            REAL(REFLD(3))*R2*CG(IK)*ABS(ECOS(ITH2X)/DELX)*ABS(R4)*FAC2 &
                          + REAL(REFLD(4))*R2*CG(IK)*ABS(ESIN(ITH2Y)/DELY)*ABS(R4)*FAC2
                      CASE (2)
                   ! straight coast: narrow reflection
! Specular for tests
!                   S(ISPEC)=S(ISPEC)+REAL(REFLD(3))*R2*CG(IK)*ABS(ECOS(ITH2)/DELX)  &
!                                    +REAL(REFLD(4))*R2*CG(IK)*ABS(ESIN(ITH2)/DELY)
!
                           S(ISPEC)=S(ISPEC)+REAL(REFLD(3))*R2*CG(IK)*ABS(ECOS(ITH2X)/DELX) &
                                                *(R4**NRS) *FAC3                          &
                                            +REAL(REFLD(4))*R2*CG(IK)*ABS(ESIN(ITH2Y)/DELY) &
                                                *(R4**NRS) *FAC3
                        END SELECT
                      END IF ! (R4.GT.0.)
                    END IF  ! (R3.LT.0)
                  END DO  ! ITH2=1,NTH
                END IF  ! (R1.GT.0.AND.R2.GT.0)
              END DO  ! ITH=1,NTH
            END IF  ! UNGTYPE
 
          END DO ! loop on IK
        END IF   ! end of test on REFLC(1)
!
!  Add diffuse reflection due to subgrid islands and icebergs
!  At present this feature is not supported for unstructured grids.
!
      IF (    ((REFPARS(2).GT.0.).AND.((TRNX+TRNY).LT.2))     &
        .OR.((REFPARS(4).GT.0.).AND.(BERG.GT.0)       )   ) THEN
!
! Includes frequency dependence (see Elgar et al. JPO 1994)
!
          IF (REFPARS(6).GT.0) THEN
            RAMP=(MAX((0.75*TPI*FMEANA/SIG(IK)),1.)**REFPARS(10))*RAMP0
            RAMP2=MIN(REFPARS(9),REFLC(2)*RAMP)
!
! recomputes coefficients for iceberg slope given by REFLC(4)
!
            SLOPE=MAX(0.001,REFLC(4))
            MICHEFAC=0.0001*GRAV**2*(SLOPE**5)  &
                   /(MAX(EMEANA,1E-4)*MAX(FMEANA,0.001)**4)
            RAMP0=MAX(0.007*(ALOG10(MICHEFAC)+4.5)+1.5*MICHEFAC,0.005)   ! IF REFLC(1)=1, 0.07 should be 0.007
            RAMP=(MAX((0.75*TPI*FMEANA/SIG(IK)),1.)**REFPARS(10))*RAMP0
            RAMP4=MIN(REFPARS(9),RAMP)
          ELSE
            RAMP2=RAMP0*REFLC(2)
            RAMP4=RAMP0*REFLC(4)
            END IF
!
        R2X=  RAMP2*REFPARS(2)*MAX(0.,MIN(1.,(1-TRNX)))  &
            + RAMP4*REFPARS(4)*MAX(0.,MIN(1.,(1-EXP(-BERG*DELX*0.0001))))
        R2Y=  RAMP2*REFPARS(2)*MAX(0.,MIN(1.,(1-TRNY)))  &
            + RAMP4*REFPARS(4)*MAX(0.,MIN(1.,(1-EXP(-BERG*DELY*0.0001))))
        FAC1=1/(0.5*REAL(NTH))
        DO IK=1, NK
          DO ITH=1, NTH
            R2=A(ITH+(IK-1)*NTH)
            IF (R2.GT.0.) THEN
 
              DO ITH2=1,NTH
                ISPEC=ITH2+(IK-1)*NTH
                R3=ECOS(1+MOD(NTH+ITH2-ITH,NTH))
                IF (R3.LT.0) THEN
                     S(ISPEC)=S(ISPEC)+ &
                               CG(IK)*R2X*R2*ABS(ECOS(ITH2)/DELX)*FAC1 &
                             + CG(IK)*R2Y*R2*ABS(ESIN(ITH2)/DELY)*FAC1
                  END IF
                END DO
              END IF
            END DO
          END DO
        END IF
 
!/
!/ End of W3SREF ----------------------------------------------------- /
!/
      END SUBROUTINE W3SREF!/ ------------------------------------------------------------------- /
 
!/
!/ End of module W3REF1MD -------------------------------------------- /
!/
      END MODULE W3REF1MD
 
 
