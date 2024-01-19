! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=none /tstdev/F2003/generic/operator/functional/genericOperatorScalar004.f
! opt variations: -qnol -qnodeferredlp -qreuse=self

!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Scalar (.eq.,.ne.,.lt.,.gt.,.le.,.ge.)
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m

   type mycomplex(n1,k1,k2)    ! (20,8,8)
      integer, kind :: k1,k2
      integer, len  :: n1
      real(k1)      :: r
      real(k2)      :: i
      contains
         procedure, pass :: myequal
         generic :: operator(.eq.) => myequal
         procedure, pass :: mynotequal
         generic :: operator(/=) => mynotequal
         procedure, pass :: mylt
         generic :: operator(<) => mylt
         procedure, pass :: mygt
         generic :: operator(>) => mygt
         procedure, pass :: myge
         generic :: operator (.ge.) => myge
         procedure, pass :: myle
         generic :: operator (.le.) => myle
   end type

   logical, external :: precision_r8

   contains

      logical function myequal ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         myequal = ( precision_r8 ( a%r, b%r ) .and.  precision_r8 ( a%i, b%i ) )

      end function

      logical function mynotequal ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         mynotequal = ( ( .not. ( a .eq. b ) ) .and. ( .not. ( a == b ) ) )

      end function

      logical function mylt ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         mylt = ( ( a%r < b%r ) .and. ( a%i .lt. b%i ) )

      end function

      logical function mygt ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         mygt = ( ( a%r > b%r ) .and. ( a%i .gt. b%i ) )

      end function

      logical function myge ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         myge = ( ( a .gt. b ) .or. ( a .eq. b ) )

      end function

      logical function myle ( a, b )
         class(mycomplex(*,8,8)), intent(in) :: a, b

         myle = ( ( a .lt. b ) .or. ( a .eq. b ) )

      end function

end module

program genericOperatorScalar004
   use m

   class(mycomplex(:,8,8)), allocatable :: c1, c11
   type(mycomplex(20,8,8)) :: c2, c21

   allocate ( c1,  source = mycomplex(20,8,8) (101.101_8, 201.201_8 ) )
   allocate ( c11, source = mycomplex(20,8,8) (100.101_8, 202.202_8 ) )

   c2  = mycomplex(20,8,8) ( 102.102_8, 202.202_8 )
   c21 = mycomplex(20,8,8) ( 103.102_8, 201.202_8 )

   if ( c2 .lt. c1 )           error stop 1_4
   if ( .not. ( c1 < c2 ) )    error stop 2_4
   if ( c1 < c11 )             error stop 3_4

   if ( c2 <= c1 )             error stop 4_4
   if ( .not. ( c2 .ge. c1 ) ) error stop 5_4

   if ( .not. ( c2 .gt. c1 ) ) error stop 6_4
   if ( c1 > c2  )             error stop 7_4
   if ( c2 > c21 )             error stop 8_4

   if ( c1 >= c2 )             error stop 9_4
   if ( .not. ( c1 .le. c2 ) ) error stop 10_4

   c2%r = c2%r -1.001_8
   c2%i = c2%i -1.001_8  !<- now should be the same as c1

   if ( c1 .ne. c2 )           error stop 11_4
   if ( .not. ( c1 == c2 ) )   error stop 12_4

   if ( .not. ( c1 .le. c2 ) ) error stop 13_4
   if ( .not. ( c1 .ge. c2 ) ) error stop 14_4

   deallocate ( c1 )
   allocate ( c1, source = mycomplex(20,8,8) ( 0.0_8, 0.0_8 ) )

   if (.not. ( c1 /= c2 ) )    error stop 15_4
   if ( c1 .eq. c2 )           error stop 16_4

   if ( .not. ( c1 .le. c2 ) ) error stop 17_4
   if ( c1 .ge. c2 )           error stop 18_4

end program
