! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : structureComp002ll
!*
!*  DATE                       : 2007-10-03 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Try output item to be an structure component for array case
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

program structureComp002ll
   use m1

   interface write(unformatted)
      subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

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
   character(27)  :: c1
   character(24)  :: c2
   character(18)  :: c3
   character(8)   :: c4
   character(36)  :: c5
   character(16)  :: c6

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


   open (unit = 1, file ='structureComp002ll.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b11                 !<- write 'abcZABCZYdefZDEFZYghiZGHIZY' to file
   write (1, iostat=stat, iomsg=msg )             b11%b1, b11%b2      !<- write 'abcZdefZghiZABCZDEFZGHIZ' to file

   write (1, iostat=stat, iomsg=msg )             b12(2:1:-1,1)       !<- write 'DEFZdefZYABCZabcZY' to file
   write (1, iostat=stat, iomsg=msg )             b12(2:1:-1,1)%b1    !<- write 'DEFZABCZ' to file

   write (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))    !<- write 'ghiZGHIZYabcZABCZYdefZDEFZYjklZJKLZY' to file
   write (1, iostat=stat, iomsg=msg )             b13((/3,1,2,4/))%b2 !<- write 'GHIZABCZDEFZJKLZ' to file

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4
   read (1, iostat=stat, iomsg=msg )              c5
   read (1, iostat=stat, iomsg=msg )              c6

   ! check if the values are set correctly

   if ( c1 /= 'abcZABCZYdefZDEFZYghiZGHIZY' )           error stop 101_4
   if ( c2 /= 'abcZdefZghiZABCZDEFZGHIZ' )              error stop 2_4
   if ( c3 /= 'DEFZdefZYABCZabcZY' )                    error stop 3_4
   if ( c4 /= 'DEFZABCZ' )                              error stop 4_4
   if ( c5 /= 'ghiZGHIZYabcZABCZYdefZDEFZYjklZJKLZY' )  error stop 5_4
   if ( c6 /= 'GHIZABCZDEFZJKLZ' )                      error stop 6_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character, intent(inout) :: iomsg

    interface write(unformatted)
        subroutine writeUnformattedBase (dtv, unit, iostat, iomsg)
            import base
            class(base(*)), intent(in) :: dtv ! tcx: (*)
            integer,  intent(in) :: unit
            integer,  intent(out) :: iostat
            character(*),  intent(inout) :: iomsg
        end subroutine
    end interface

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%b1
    write (unit, iostat=iostat, iomsg=iomsg ) dtv%b2
    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Y"

end subroutine

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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 28 changes
! type: container - added parameters (lcontainer_1) to invoke with (3) / declare with (*) - 16 changes
