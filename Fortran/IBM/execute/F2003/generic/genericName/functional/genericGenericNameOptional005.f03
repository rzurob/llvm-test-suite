!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: optional dummy arguments with arrays
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

   type :: base
      integer(4) :: i
      contains
         procedure, pass :: b_barray_barray
         procedure, pass :: b_b_barray
         generic :: print => b_barray_barray, b_b_barray

         procedure, pass :: writebase
         generic :: write(formatted) => writebase
   end type

   type, extends(base) :: child
      integer :: j
      contains
         procedure, pass :: b_barray_barray => c_barray_barray
         procedure, pass :: b_b_barray => c_b_barray
         procedure, nopass :: carray_b_barray
         generic :: print => carray_b_barray
   end type

   contains

      subroutine writebase( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: v_list
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base )
               write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i
            type is ( child )
               write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i, dtv%j
         end select

      end subroutine

      subroutine b_barray_barray( a, b, c )
         class(base), intent(in) :: a, b(:), c(:)
         optional :: c

         print *, "xxxxxx  b_barray_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine b_b_barray( a, b, c )
         class(base), intent(in) :: a, b, c(:)
         optional :: c

         print *, "xxxxxx  b_b_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine c_barray_barray( a, b, c )
         class(child), intent(in) :: a
         class(base), intent(in) :: b(:), c(:)
         optional :: c

         print *, "xxxxxx  c_barray_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine c_b_barray( a, b, c )
         class(child), intent(in) :: a
         class(base), intent(in) :: b, c(:)
         optional :: c

         print *, "xxxxxx  c_b_barray  xxxxxx"
        if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine carray_b_barray( a, b, c )
         class(child), intent(in) :: a(:), b, c(:)
         optional :: c

         print *, "xxxxxx  carray_b_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

end module

program genericGenericNameOptional005
   use m

   class(base), allocatable :: b1, b2(:), b3(:)
   class(child), pointer :: c1, c2(:), c3(:)

   allocate ( b1, source = base(100) )
   allocate ( b2(5), source = (/ ( base(100*i), i = 2, 6 ) /) )
   allocate ( b3(5), source = (/ ( base(100*i), i = 7, 11 ) /) )

   allocate ( c1, source = child(1000, 1001) )
   allocate ( c2(5), source = (/ ( child(1000*i,1001*i), i = 2, 6 ) /) )
   allocate ( c3(5), source = (/ ( child(1000*i, 1001*i), i = 7, 11 ) /) )

   call b1%print(b1, b2)
   call b1%print(b2, b3)
   call b1%print(c1, c2)
   call b1%print(c2, c3)

   call c1%print(b1, b2)
   call c1%print(b2, b3)
   call c1%print(c1, c2)
   call c1%print(c2, c3)

   deallocate ( b1 )
   allocate ( b1, source = c1 )

   call b1%print(b1, b2)
   call b1%print(b2, b3)
   call b1%print(c1, c2)
   call b1%print(c2, c3)

   call c2%print(c2, c1, c3)

end program