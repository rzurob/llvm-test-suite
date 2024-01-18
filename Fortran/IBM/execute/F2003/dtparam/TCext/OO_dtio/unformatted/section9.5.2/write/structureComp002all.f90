! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an structure component for array case (without container interface)
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

   type base (lbase_1) ! lbase_1=3
      integer, len :: lbase_1
      character(lbase_1) :: c = ''
      contains
         procedure, pass :: getC
   end type

   type container (lcontainer_1) ! lcontainer_1=3
      integer, len :: lcontainer_1
      type(base(lcontainer_1)) :: b1 ! tcx: (lcontainer_1)
      type(base(lcontainer_1)) :: b2 ! tcx: (lcontainer_1)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module

program structureComp002all
   use m1

   interface write(unformatted)
      subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   type(container(:)), allocatable  :: b11(:) ! tcx: (:)
   type(container(:)), pointer      :: b12(:,:) ! tcx: (:)
   type(container(3))               :: b13(4) ! tcx: (3)
   integer :: stat
   character(200) :: msg
   character(24)  :: c1, c2
   character(16)  :: c3, c6
   character(8)   :: c4
   character(32)  :: c5

   ! allocation of variables
   allocate ( b11(3), source = (/ container(3)( b2=base(3)('ABC'), b1=base(3)('abc') ),       &  ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                  container(3)( b2=base(3)('DEF'), b1=base(3)('def') ),       & ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                  container(3)( b2=base(3)('GHI'), b1=base(3)('ghi') ) /) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   allocate ( b12(2,2), source = reshape ( source = (/ container(3)( b2=base(3)('abc'), b1=base(3)('ABC') ),    & ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                                       container(3)( b2=base(3)('def'), b1=base(3)('DEF') ),    & ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                                       container(3)( b2=base(3)('ghi'), b1=base(3)('GHI') ),    & ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                                       container(3)( b2=base(3)('jkl'), b1=base(3)('JKL') ) /), & ! tcx: (3) ! tcx: (3) ! tcx: (3)
                                                       shape=(/2,2/) ) )

   b13 = (/ container(3)( b2=base(3)('ABC'), b1=base(3)('abc') ),       &  ! tcx: (3) ! tcx: (3) ! tcx: (3)
            container(3)( b2=base(3)('DEF'), b1=base(3)('def') ),       & ! tcx: (3) ! tcx: (3) ! tcx: (3)
            container(3)( b2=base(3)('GHI'), b1=base(3)('ghi') ),       & ! tcx: (3) ! tcx: (3) ! tcx: (3)
            container(3)( b2=base(3)('JKL'), b1=base(3)('jkl') )     /) ! tcx: (3) ! tcx: (3) ! tcx: (3)


   open (unit = 1, file ='structureComp002all.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b11                 !<- write 'abcZABCZdefZDEFZghiZGHIZ' to file
   write (1, iostat=stat, iomsg=msg )             b11%b1, b11%b2      !<- write 'abcZdefZghiZABCZDEFZGHIZ' to file

   write (1, iostat=stat, iomsg=msg )             b12(2:1:-1,1)       !<- write 'DEFZdefZABCZabcZ' to file
   write (1, iostat=stat, iomsg=msg )             b12(2:1:-1,1)%b1    !<- write 'DEFZABCZ' to file

   write (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))    !<- write 'ghiZGHIZabcZABCZdefZDEFZjklZJKLZ' to file
   write (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))%b2 !<- write 'GHIZABCZDEFZJKLZ' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5
   read (1, iostat=stat, iomsg=msg )              c6

   ! check if the values are set correctly

   if ( c1 /= 'abcZABCZdefZDEFZghiZGHIZ' )           error stop 101_4
   if ( c2 /= 'abcZdefZghiZABCZDEFZGHIZ' )           error stop 2_4
   if ( c3 /= 'DEFZdefZABCZabcZ' )                   error stop 3_4
   if ( c4 /= 'DEFZABCZ' )                           error stop 4_4
   if ( c5 /= 'ghiZGHIZabcZABCZdefZDEFZjklZJKLZ' )   error stop 5_4
   if ( c6 /= 'GHIZABCZDEFZJKLZ' )                   error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%getC()
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"
end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 27 changes
! type: container - added parameters (lcontainer_1) to invoke with (3) / declare with (*) - 14 changes
