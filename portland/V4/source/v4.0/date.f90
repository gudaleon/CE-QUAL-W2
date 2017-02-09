
!***********************************************************************************************************************************
!**                                     S U B R O U T I N E    G R E G O R I A N   D A T E                                        **
!***********************************************************************************************************************************

SUBROUTINE GREGORIAN_DATE
  USE GDAYC
  IMPLICIT NONE
  INTEGER :: INCR

! Determine if new year (regular or leap) and increment year

  DO WHILE (JDAYG >= 366)
    IF (.NOT. LEAP_YEAR .AND. JDAYG >= 366) THEN
      JDAYG     = JDAYG-365
      YEAR      = YEAR+1
      LEAP_YEAR = MOD(YEAR,4) == 0
    ELSE IF (JDAYG >= 367) THEN
      JDAYG     = JDAYG-366
      YEAR      = YEAR+1
      LEAP_YEAR = MOD(YEAR,4) == 0
    ELSE
      EXIT
    END IF
  END DO
  INCR = 0
  IF (LEAP_YEAR) INCR = 1

! Determine month and day of year

  IF (JDAYG >= 1 .AND. JDAYG < 32) THEN
    GDAY  = JDAYG
    DAYM  = 31.0
    MONTH = '  January'
    M     = 1
  ELSE IF (JDAYG >= 32 .AND. JDAYG < 60+INCR) THEN
    GDAY  = JDAYG-31
    DAYM  = 29.0
    MONTH = ' February'
    M     = 2
  ELSE IF (JDAYG >= 60 .AND. JDAYG < 91+INCR) THEN
    GDAY  = JDAYG-59-INCR
    DAYM  = 31.0
    MONTH = '    March'
    M     = 3
  ELSE IF (JDAYG >= 91 .AND. JDAYG < 121+INCR) THEN
    GDAY  = JDAYG-90-INCR
    DAYM  = 30.0
    MONTH = '    April'
    M     = 4
  ELSE IF (JDAYG >= 121 .AND. JDAYG < 152+INCR) THEN
    GDAY  = JDAYG-120-INCR
    DAYM  = 31.0
    MONTH = '      May'
    M     = 5
  ELSE IF (JDAYG >= 152 .AND. JDAYG < 182+INCR) THEN
    GDAY  = JDAYG-151-INCR
    DAYM  = 30.0
    MONTH = '     June'
    M     = 6
  ELSE IF (JDAYG >= 182 .AND. JDAYG < 213+INCR) THEN
    GDAY  = JDAYG-181-INCR
    DAYM  = 31.0
    MONTH = '     July'
    M     = 7
  ELSE IF (JDAYG >= 213 .AND. JDAYG < 244+INCR) THEN
    GDAY  = JDAYG-212-INCR
    DAYM  = 31.0
    MONTH = '   August'
    M     = 8
  ELSE IF (JDAYG >= 244 .AND. JDAYG < 274+INCR) THEN
    GDAY  = JDAYG-243-INCR
    DAYM  = 30.0
    MONTH = 'September'
    M     = 9
  ELSE IF (JDAYG >= 274 .AND. JDAYG < 305+INCR) THEN
    GDAY  = JDAYG-273-INCR
    DAYM  = 31.0
    MONTH = '  October'
    M     = 10
  ELSE IF (JDAYG >= 305 .AND. JDAYG < 335+INCR) THEN
    GDAY  = JDAYG-304-INCR
    DAYM  = 30.0
    MONTH = ' November'
    M     = 11
  ELSE IF (JDAYG >= 335 .AND. JDAYG < 366+INCR) THEN
    GDAY  = JDAYG-334-INCR
    DAYM  = 31.0
    MONTH = ' December'
    M     = 12
  END IF
END SUBROUTINE GREGORIAN_DATE
