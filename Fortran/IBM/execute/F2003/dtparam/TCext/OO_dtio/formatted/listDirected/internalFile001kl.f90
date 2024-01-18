! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : internalFile001kl
!*
!*  DATE                       : 2007-06-20 (original: 11/04/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data Transfer Statements
!*                                       - io-unit is internal file, and try scalar read/write operations
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
    type :: base (lb)
       integer, len :: lb
        character(lb) :: c
    end type

end module

program internalFile001kl
use m

   interface write(formatted)
      subroutine writeFormatted (dtv, unit, iotype, v_list, iostat, iomsg)
      import base
         class (base(*)), intent(in) :: dtv
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
         class (base(*)), intent(inout) :: dtv
         integer, intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)  :: v_list(:)
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   ! Declaration of variables

   class(base(:)), allocatable :: b1
   class(base(:)), pointer     :: b2
   type(base(3))               :: b3

   class(base(:)), allocatable :: b11
   class(base(:)), pointer     :: b21
   type(base(3))               :: b31

   ! allocation of varaibles

   character(4), dimension(10) :: internalFile

   allocate (b1, source= base(3)('ABC'))
   allocate (b2, source= base(3)('DEF'))
   b3 = base(3) ('GHI')
   allocate (base(3):: b11, b21)

   ! I/O operations

   write(internalFile(1),*) b1
   write(internalFile(2),*) b2
   write(internalFile(3),*) b3

   read(internalFile(3),*)  b11
   read(internalFile(2),*)  b21
   read(internalFile(1),*)  b31

   if ( b11%c /= 'GHI' ) error stop 3_4
   if ( b21%c /= 'DEF' ) error stop 4_4
   if ( b31%c /= 'ABC' ) error stop 5_4

end program


subroutine writeFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m
    class(base(*)), intent(in) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (unit >= 0) error stop 1_4

    write (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine

subroutine readFormatted (dtv, unit, iotype, v_list,iostat, iomsg)
use m
    class(base(*)), intent(inout) :: dtv
    integer, intent(in) :: unit
    character(*), intent(in) :: iotype
    integer, intent(in)  :: v_list(:)
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    if (unit >= 0) error stop 2_4

    read (unit, *, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine
