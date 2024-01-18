!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 05/01/2006
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Characters with deferred length type parameter
!*
!*  DRIVER STANZA              : xlf2003
!*
!*  DESCRIPTION                : scalar character with deferred length
!*                               of arrays and formatted dt I/O
!*
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

   character(len=: ), pointer :: c1(:,:)

   type base
      character(:), pointer :: c(:)
      contains
         procedure :: write
         procedure :: read
         generic :: write(formatted) => write
         generic :: read(formatted) => read
   end type


   contains

      subroutine write ( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base), intent(in) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         write ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

      subroutine read ( dtv, unit, iotype, v_list, iostat, iomsg )
         class(base), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in) :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg

         read ( unit, *, iostat = iostat, iomsg = iomsg ) dtv%c

      end subroutine

end module

program deferLenPtrAssgn017
   use m

   implicit type(base) (b)
   character(3) :: c(3,3)
   allocatable :: b1

   character(:), allocatable, target :: ccc(:), cc(:,:)

   allocate ( ccc(4), source = (/ 'ABC', 'DEF', 'GHI', 'JKL' /) )

   allocate ( cc(2,2), source = reshape ( source = (/ 'abc', 'def', 'ghi', 'jkl' /) , shape = (/2,2/) ) )

   c1 => cc

   allocate ( b1 )
   b1%c => ccc

   c = reshape ( source = (/ c1(1:2,1) , c1(1:2,2) , b1%c((/ 4,3,4,2,1 /))  /) , shape = (/3,3/) )

   print *, c1, len(c1)
   print *, b1, len(b1%c)
   print *, c, len(c)

end program
