!*  ===================================================================
!*
!*  DATE                       : 11/01/2005
!*
!*  PRIMARY FUNCTIONS TESTED   : Section 4.5.4: Generic Type Bound Procedure
!*  SECONDARY FUNCTIONS TESTED : with generic-name
!*
!*  DESCRIPTION                : generic-name: optional dummy arguments
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
      character(2):: c
      integer(4) :: i
      contains
         procedure, pass :: fiveoptional
         generic :: print => fiveoptional
         procedure, pass :: writebase
         generic :: write(formatted) => writebase
   end type

   contains

      subroutine writebase( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, dimension(:), intent(in) :: v_list
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, "(1x,A2,':',I4)", iostat = iostat, iomsg = iomsg ) dtv%c, dtv%i

      end subroutine

      subroutine fiveoptional ( a, b, c, d, e, f )
         class(base), intent(in) :: a
         class(base), intent(in), optional :: b,c,d,e,f

         write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
         write (*,*) a

         if ( present(b) ) write (*,*) b
         if ( present(c) ) write (*,*) c
         if ( present(d) ) write (*,*) d
         if ( present(e) ) write (*,*) e
         if ( present(f) ) write (*,*) f

         write (*,*) "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"

      end subroutine

end module

program genericGenericNameOptional001
   use m

   class(base), allocatable :: b1, b2
   class(base), pointer     :: b3, b4
   type(base)               :: b5, b6

   allocate ( b1, source = base('b1', 100) )
   allocate ( b2, source = base('b2', 200) )
   allocate ( b3, source = base('b3', 300) )
   allocate ( b4, source = base('b4', 400) )

   b5= base('b5', 500)
   b6= base('b6', 600)

   call b1%print()
   call b2%print()
   call b3%print()
   call b4%print()
   call b5%print()
   call b6%print()

   call b1%print(b2)
   call b2%print(b3)
   call b3%print(b4)
   call b4%print(b5)
   call b5%print(b6)

   call b1%print(b2,b3)
   call b2%print(b3,b4)
   call b3%print(b4,b5)
   call b4%print(b5,b6)

   call b1%print(b2,b3,b4)
   call b2%print(b3,b4,b5)
   call b3%print(b4,b5,b6)

   call b1%print(b2,b3,b4,b5)
   call b2%print(b3,b4,b5,b6)

   call b1%print(b2,b3,b4,b5,b6)

   call b1%print(f=b2)
   call b2%print(c=b3,e=b4)
   call b3%print(e=b5,d=b4)

end program
