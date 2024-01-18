!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            : GENERICS
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic name
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : Generic type bound call appearing in spec-expr for array bounds
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
         procedure, pass :: puregeti
         procedure, pass :: proc_4
         generic :: procs => proc_4
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: proc_4_2
         generic :: procs => proc_4_2
   end type

   contains

      integer pure function puregeti ( dtv )
         class(base), intent(in) :: dtv

         puregeti = dtv%i

      end function

      subroutine proc_4 ( dtv, i )
         class(base), intent(inout) :: dtv
         integer(4), intent(in) :: i(dtv%puregeti():dtv%puregeti()+4)

         do k = lbound(i,1),ubound(i,1)
            dtv%i = dtv%i + i(k)
         end do

      end subroutine

      subroutine proc_4_2 ( dtv, i, j )
         class(child), intent(inout) :: dtv
         integer(4), intent(in) :: i(dtv%puregeti():dtv%puregeti()+9)
         integer(4), intent(in) :: j(dtv%puregeti()+100:dtv%puregeti()+109)
         
         do k = lbound(i,1),ubound(i,1)
            dtv%i = dtv%i + i(k)
         end do

         do k = lbound(j,1),ubound(j,1)
            dtv%j = dtv%j + j(k)
         end do

      end subroutine

end module

program genericGenericNameSpecExpr001
   use m

   class(base), allocatable :: b1
   type(child) :: c1
   integer, parameter :: j(100) = 1

   allocate ( b1, source = base(123) )
   c1 = child(246,369)

   call b1%procs(j)
   if ( b1%i /= 128 ) error stop 1_4

   call c1%procs( j(50:) )
   if ( ( c1%i /= 251 ) .or. ( c1%j /= 369 ) ) error stop 2_4

   call c1%procs( j(50:), j(10:1:-1) )
   if ( ( c1%i /= 261 ) .or. ( c1%j /= 379 ) ) error stop 3_4

   deallocate ( b1 )
   allocate ( b1, source = child (100,200) )

   call b1%procs(j)
   select type ( b1 )
      type is ( child )
         if ( ( b1%i /= 105 ) .or. ( b1%j /= 200 ) ) error stop 4_4
      class default
         error stop 5_4
   end select

   select type ( b1 )
      type is ( child )
         call b1%procs((/2,2,2,2,2,2,2,2,2,2/),j((/1,3,5,7,9,11,13,17,19,21/)))
         if ( ( b1%i /= 125 ) .or. ( b1%j /= 210 ) ) error stop 6_4
      class default
         error stop 7_4
   end select

end program
