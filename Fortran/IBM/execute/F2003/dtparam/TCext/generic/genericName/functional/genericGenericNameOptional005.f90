! GB DTP extension using:
! ftcx_dtp -ql -qdeferredlp -qreuse=base /tstdev/F2003/generic/genericName/functional/genericGenericNameOptional005.f
! opt variations: -qnol -qnodeferredlp -qreuse=none

!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/01/2005
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*                             :
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DRIVER STANZA              : xlf2003
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

   type :: base(n1,k1)    ! (20,4)
      integer, kind :: k1
      integer, len  :: n1
      integer(k1)   :: i
      contains
         procedure, pass :: b_barray_barray
         procedure, pass :: b_b_barray
         generic :: print => b_barray_barray, b_b_barray

         procedure, pass :: writebase
         generic :: write(formatted) => writebase
   end type

   type, extends(base) :: child    ! (20,4)
      integer(k1) :: j
      contains
         procedure, pass :: b_barray_barray => c_barray_barray
         procedure, pass :: b_b_barray => c_b_barray
         procedure, nopass :: carray_b_barray
         generic :: print => carray_b_barray
   end type

   contains

      subroutine writebase( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base(*,4)), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: v_list
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         select type ( dtv )
            type is ( base(*,4) )
               write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i
            type is ( child(*,4) )
               write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%i, dtv%j
         end select

      end subroutine

      subroutine b_barray_barray( a, b, c )
         class(base(*,4)), intent(in) :: a, b(:), c(:)
         optional :: c

         print *, "xxxxxx  b_barray_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine b_b_barray( a, b, c )
         class(base(*,4)), intent(in) :: a, b, c(:)
         optional :: c

         print *, "xxxxxx  b_b_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine c_barray_barray( a, b, c )
         class(child(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b(:), c(:)
         optional :: c

         print *, "xxxxxx  c_barray_barray  xxxxxx"
         if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine c_b_barray( a, b, c )
         class(child(*,4)), intent(in) :: a
         class(base(*,4)), intent(in) :: b, c(:)
         optional :: c

         print *, "xxxxxx  c_b_barray  xxxxxx"
        if ( present(c) ) then
            print *, a, b, c
         else
            print *, a, b
         end if

      end subroutine

      subroutine carray_b_barray( a, b, c )
         class(child(*,4)), intent(in) :: a(:), b, c(:)
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

   class(base(:,4)), allocatable :: b1, b2(:), b3(:)
   class(child(:,4)), pointer :: c1, c2(:), c3(:)

   allocate ( b1, source = base(20,4)(100) )
   allocate ( b2(5), source = (/ ( base(20,4)(100*i), i = 2, 6 ) /) )
   allocate ( b3(5), source = (/ ( base(20,4)(100*i), i = 7, 11 ) /) )

   allocate ( c1, source = child(20,4)(1000, 1001) )
   allocate ( c2(5), source = (/ ( child(20,4)(1000*i,1001*i), i = 2, 6 ) /) )
   allocate ( c3(5), source = (/ ( child(20,4)(1000*i, 1001*i), i = 7, 11 ) /) )

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
