! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : structureConstr001lk
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an structure constructor
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
      real(kbase2_1) :: r
   end type

   type base3 (kbase3_1,lbase3_2) ! kbase3_1,lbase3_2=4,2
      integer, kind :: kbase3_1
      integer, len :: lbase3_2
      integer(kbase3_1) :: i(lbase3_2)
   end type

end module


program structureConstr001lk
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

      subroutine writeUnformatted3 (dtv, unit, iostat, iomsg)
         import base3
         class(base3(4,*)), intent(in) :: dtv ! tcx: (4,*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables

   integer :: stat
   character(200) :: msg
   character(4) :: c1
   integer, dimension(2) :: i1
   real(4) :: r1

   logical precision_r4

   ! allocation of variables

   open (unit = 1, file ='structureConstr001lk.data', form='unformatted', access='sequential')

   ! I/O operations

   write (1, iostat=stat, iomsg=msg )      base1(3)('abcZ') ! tcx: (3)
   write (1, iostat=stat, iomsg=msg )      base2(4)(5.0) ! tcx: (4)
   write (1, iostat=stat, iomsg=msg )      base3(4,2)((/6,7/)) ! tcx: (4,2)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1              !<- should read 'abc'
   read (1, iostat=stat, iomsg=msg )              r1              !<- should read 6.0
   read (1, iostat=stat, iomsg=msg )              i1              !<- should read 7 and 8

   ! check if the values are set correctly

   if ( c1 /= 'abcZ' )                        error stop 101_4
   if ( .not. precision_r4(r1, 6.0)  )        error stop 2_4
   if ( ( i1(1) /= 7 ) .or. ( i1(2) /= 8 ) )  error stop 3_4

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

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%r + 1.0

end subroutine

subroutine writeUnformatted3 (dtv, unit, iostat, iomsg)
use m1
    class(base3(4,*)), intent(in) :: dtv ! tcx: (4,*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%i + 1

end subroutine


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (lbase1_1) to invoke with (3) / declare with (*) - 3 changes
! type: base2 - added parameters (kbase2_1) to invoke with (4) / declare with (4) - 3 changes
! type: base3 - added parameters (kbase3_1,lbase3_2) to invoke with (4,2) / declare with (4,*) - 3 changes
