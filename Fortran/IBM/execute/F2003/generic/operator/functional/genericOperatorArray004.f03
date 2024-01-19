!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with Operator( )
!*
!*  DESCRIPTION                : Binary Operator: Scalar to Array (.eq.,.ne.,.lt.,.gt.,.le.,.ge.)
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

   type base
      integer :: i
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

   logical, external :: precision_r4

   contains

      logical function myequal ( a, b )
         class(base), intent(in) :: a, b(:)

         myequal = .true.

         do i = 1, size(b)
            myequal = myequal .and. ( a%i .eq. b(i)%i )
         end do

      end function

      logical function mynotequal ( a, b )
         class(base), intent(in) :: a, b(:)

         mynotequal = ( .not. ( a == b ) )

      end function

      logical function mylt ( a, b )
         class(base), intent(in) :: a, b(:)

         mylt = .true.

         do i = 1, size(b)
            mylt = mylt .and. ( a%i < b(i)%i )
         end do

      end function

      logical function mygt ( a, b )
         class(base), intent(in) :: a, b(:)

         mygt = .true.

         do i = 1, size(b)
            mygt = mygt .and. ( a%i > b(i)%i )
         end do

      end function

      logical function myge ( a, b )
         class(base), intent(in) :: a, b(:)

         myge = ( ( a .gt. b ) .or. ( a .eq. b ) )

      end function

      logical function myle ( a, b )
         class(base), intent(in) :: a, b(:)

         myle = ( ( a .lt. b ) .or. ( a .eq. b ) )

      end function

end module

program genericOperatorArray004
   use m

   class(base), allocatable :: c1, c11(:)

   allocate ( c1,     source = base (100) )
   allocate ( c11(4), source = (/ base (10),  &
                                  base (20),  &
                                  base (30), base(40)   /) )

   if ( c1 .lt. c11 )           error stop 1_4
   if ( c1 < c11 )              error stop 2_4

   if ( c1 <= c11 )             error stop 3_4
   if ( c1 .le. c11 )           error stop 4_4

   if (.not. ( c1 >= c11 ) )    error stop 5_4
   if (.not. ( c1 .ge. c11 ) )  error stop 6_4

   if (.not. ( c1 > c11 ) )     error stop 7_4
   if (.not. ( c1 .gt. c11 ) )  error stop 8_4

   if ( c1 == c11 )             error stop 9_4
   if ( c1 .eq. c11 )           error stop 10_4

   if ( .not. (  c1 /= c11  ))  error stop 11_4
   if ( .not. ( c1 .ne. c11 ))  error stop 12_4

   deallocate ( c11 )
   allocate ( c11(5), source = (/ (c1, i=1,5) /) )

   if ( c1 .lt. c11 )           error stop 13_4
   if ( c1 < c11 )              error stop 14_4

   if ( .not. ( c1 <= c11 ) )   error stop 15_4
   if ( .not. ( c1 .le. c11 ) ) error stop 16_4

   if ( .not. ( c1 >= c11 ) )   error stop 17_4
   if ( .not. ( c1 .ge. c11 ) ) error stop 18_4

   if ( c1 > c11 )              error stop 19_4
   if (c1 .gt. c11 )            error stop 20_4

   if (.not.( c1 == c11 ))      error stop 21_4
   if (.not.( c1 .eq. c11 ))    error stop 22_4

   if ( c1 /= c11 )             error stop 23_4
   if ( c1 .ne. c11 )           error stop 24_4

end program
