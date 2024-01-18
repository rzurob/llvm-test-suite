! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an array constructor with reshape intrinsic
!*                               Sequential Access
!*  KEYWORD(S)                 :
!*  TARGET(S)                  :
!* ===================================================================
!*
!*  REVISION HISTORY
!*
!*  MM/DD/YY:  Init:  Comments:
!* ===================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

module m1
   type base1 (lbase1_1) ! lbase1_1=3
      integer, len :: lbase1_1
      character(lbase1_1) :: c = ''
   end type

   type base2 (kbase2_1) ! kbase2_1=4
      integer, kind :: kbase2_1
      integer(kbase2_1) :: i
   end type

end module


program arrayConstr001alk
   use m1

   interface write(unformatted)
      subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

      subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
         import base2
         class(base2(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character(16) :: c1
   integer :: j1(4)

   ! allocation of variables

   open (unit = 1, file ='arrayConstr001alk.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      reshape( source = (/ base1(3)('abc'), base1(3)('def'), base1(3)('ghi'), base1(3)('jkl') /), shape = (/2,2/) ) ! tcx: (3) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   write (1, iostat=stat, iomsg=msg )      reshape( source = (/ (base2(4)(i),i = 4,7) /), shape=(/1,4/) ) ! tcx: (4)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              j1      !<- should read (5,6,7,8)

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZghiZjklZ' )                                                     error stop 101_4
   if ( ( j1(1) /= 5 ) .or. ( j1(2) /= 6 ) .or. ( j1(3) /= 7 ) .or. ( j1(4) /= 8 ) )     error stop 2_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted1 (dtv, unit, iostat, iomsg)
use m1
    class(base1(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine

subroutine writeUnformatted2 (dtv, unit, iostat, iomsg)
use m1
    class(base2(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i + 1

end subroutine


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (lbase1_1) to invoke with (3) / declare with (*) - 6 changes
! type: base2 - added parameters (kbase2_1) to invoke with (4) / declare with (4) - 3 changes
