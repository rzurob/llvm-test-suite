! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-09-09 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statements
!*                               - C919: if io-unit is not a file-unit-number, then no POS= and REC=
!*                                  - io-unit is *, and specify POS= or REC=
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
    type :: base (lbase_1) ! lbase_1=3
       integer, len :: lbase_1
        character(lbase_1) :: c
    end type
end module

program C919_001bl
use m

   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(in) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface read(formatted)
      subroutine readFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(inout) :: dtv ! tcx: (*)
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   class(base(:)), allocatable :: b1 ! tcx: (:)
   class(base(:)), pointer     :: b2 ! tcx: (:)

   allocate (b1, source= base(3)('IBM')) ! tcx: (3)
   allocate (base(3)::b2) ! tcx: base(3)

   write(*,*, pos=1) b1           !<- illegal to specify pos=
   read (*,*, pos=1) b2           !<- illegal to specify pos=

end program


subroutine writeFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine

subroutine readFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine



! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
