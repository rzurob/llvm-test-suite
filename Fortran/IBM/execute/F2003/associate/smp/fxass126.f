! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : Feb 5,2004
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
!*  EaECUTABLE                 : bes
!*
!*  INPUTS                     : None
!*  OUTPUTS                    : None
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
!*                                    and Reduction.using real, integer
!*                                    complex data types.
!* ===================================================================
!*
!*  REVISION HISTORb
!*
!*  MM/DD/bY:  Init:  Comments:
!*                    -Initial Version
!*
!* ===================================================================
!234567890123456789012345678901234567890123456789012345678901234567890

      program fxass126
      implicit none

      integer  i, j, k
      real, allocatable :: a(:), b(:)
      complex, allocatable :: c(:)
      real  var
      logical  precision_r4, precision_x8

        var = 0.0

      do i= 1, 10
        allocate(a(i))
        allocate(b(i))
        allocate(c(i))
        a = 1.0
        b = 1.0
        c = (1.0,1.0)

!SMP$ parallel do default(private), shared(a,b,i,c), firstprivate(var)
        do j=1, i
          associate ( arg => var )
          if (.not. precision_r4(arg,var)) error stop 1

          end associate

          associate ( arg1 => a(i) + b(i) )
          if (.not. precision_r4(arg1,(a(i) + b(i)))) error stop 2
          end associate

          associate ( arg2 => c(j) )
          arg2 = arg2 + j*2
          if (.not. precision_x8(arg2,c(j))) error stop 3
          end associate

          associate ( arg3 => var + a(i)*b(i))
          if (.not. precision_r4(arg3,(var + a(i)*b(i)))) error stop 4
          end associate

          associate ( arg4 => var )
          arg4 = a(i)*b(i)
          if (.not. precision_r4(arg4,var)) error stop 5
          end associate

        enddo

        deallocate(a,b,c)
      enddo

      end
