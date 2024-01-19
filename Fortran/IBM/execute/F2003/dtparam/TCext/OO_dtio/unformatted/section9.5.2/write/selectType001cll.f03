! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer Input/Output list
!*                               - try to write associate name (from select type construct)
!*                                 with unlimited polymorphic arrays being the selector
!*                               Direct Access
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

module m1
   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
   end type

   type, extends(base) :: child (lchild_1) ! lchild_1=3
      integer, len :: lchild_1
      character(lchild_1) :: cc = ''
   end type

end module

program selectType001cll
   use m1

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         use m1
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(*), pointer      :: b1(:)
   class(*), allocatable  :: b2(:,:)
   class(*), allocatable  :: b3(:)
   integer :: stat
   character(200) :: msg = ''
   character(14) :: c1
   character(28) :: c2
   character(8) :: c3

   ! allocation of variables

   allocate ( b1(3), source = (/ child(3,3)('abc', 'def'), child(3,3)('ghi', 'jkl'), child(3,3)('mno', 'pqr') /) ) ! tcx: (3,3) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b2(2,2), source = reshape( source = (/ child(3,3)('ABC','DEF'), child(3,3)('GHI','JKL'), & ! tcx: (3,3) ! tcx: (3,3)
                                child(3,3)('MNO','PQR'), child(3,3)('STU','VWX') /), shape = (/2,2/)) ) ! tcx: (3,3) ! tcx: (3,3)
   allocate ( b3(3), source = (/ base(3)('abc'), base(3)('def'), base(3)('ghi') /)) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='selectType001cll.data', form='unformatted', access='direct', recl=50)

   ! unformatted I/O operations

   select type (b11 => b1(1:3:2) )
      class is (base(*)) ! tcx: (*)
         write (1, iostat=stat, iomsg=msg, rec=3 )    b11
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 101_4
      class default
         error stop 2_4
   end select

   select type (b12 => b2)
      class is (child(*,*)) ! tcx: (*,*)
         write (1, iostat=stat, iomsg=msg, rec=2 )    b12
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 3_4
      class default
         error stop 4_4
   end select

   select type (b13 => b3((/3,1/)) )
      class is (base(*)) ! tcx: (*)
         write (1, iostat=stat, iomsg=msg, rec=1 )    b13
         if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 5_4
      class default
         error stop 6_4
   end select

   read (1, iostat=stat, iomsg=msg, rec=3 )       c1
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 7_4
   read (1, iostat=stat, iomsg=msg, rec=2 )       c2
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 8_4
   read (1, iostat=stat, iomsg=msg, rec=1 )       c3
   if ( ( stat /= 0 ) .or. (msg /= '') ) error stop 9_4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefZmnopqrZ' )                      error stop 10_4
   if ( c2 /= 'ABCDEFZGHIJKLZMNOPQRZSTUVWXZ' )        error stop 11_4
   if ( c3 /= 'ghiZabcZ' )                            error stop 12_4

   !close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 ) error stop 7_4

    select type (dtv)
       type is (child(*,*)) ! tcx: (*,*)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 7 changes
! type: child - added parameters (lchild_1) to invoke with (3,3) / declare with (*,*) - 9 changes
