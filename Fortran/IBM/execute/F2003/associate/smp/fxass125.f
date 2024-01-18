! *********************************************************************
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
! *********************************************************************
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD:
! %COMPOPTS: -qsmp -F:xlf90_r -qfixed 
! %GROUP: fxass125.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  AIa XL FORTRAN/6000 TEST CASE                 IBM INTERNAL USE ONLb
!*  ===================================================================
!*
!*  TEST CASE NAME             : fxass125.f
!*  TEST CASE TITLE            : ASSOCIATE
!*
!*  PROGRAMMER                 : Sarah Kouchaki-Ramezan
!*  DATE                       : Feb 5,2004
!*  ORIGIN                     : AIa Compiler Development, Toronto Lab
!*
!*  PRIMARb FUNCTIONS TESTED   : ASSOCIATE with PARALLEL DO, SHARED
!*                               PRIVATE, REDUCTION
!*  SECONDARb FUNCTIONS TESTED : None
!*
!*  KEbWORD(S)                 :
!*  TARGET(S)                  :
!*  NUMBER OF TESTS            : 1
!*  STATUS                     : done
!*
!*  STRUCTURE                  : Main program
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
!*
!*
!*  SETUP REQUIREMENTS         : N/A
!*  DEPENDENCIES               : External routine cZRC
!*  REQUIRED COMPILER OPTIONS  : None
!*
!*  NORMAL COMPLETION          : Return code = 0
!*  ABNORMAL COMPLETION        : Return code ^= 0
!*
!*  RUN TIME ESTIMATE          : <60 SECS
!*
!*  CONDITIONS TESTED          : Listed below.
!*
!*  DESCRIPTION               : Test: ASSOCIATE with Parallel DO,
!*                                    the SHARE-clause, Private
!*                                    and Reduction.using logical
!*                                    data type.
!* ===================================================================
!*
!*  REVISION HISTORb
!*
!*  MM/DD/bY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================

      program fxass125
      implicit none

      integer  i
      logical  log1(10), log2(10)
      
      log1 = .false. 
      log2 = .true.

!SMP$ parallel do default(private), share(log1,log2)
      do i=1, 10
        associate ( arg => log1(i) )
         if (arg .neqv. log1(i)) error stop 1
        end associate

        associate ( arg => (log2(i) .eqv. log1(i)))
         if (arg .neqv. (log2(i) .eqv. log1(i))) error stop 2
        end associate
 
        associate ( arg2 => log2(i) )
         arg2 = arg2 .eqv. log1(i)
         if (arg2 .neqv. log2(i)) error stop 3
        end associate 

      enddo
         
      end
