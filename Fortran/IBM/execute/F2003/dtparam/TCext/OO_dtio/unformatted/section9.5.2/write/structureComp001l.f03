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
!*                               - Try output item to be structure component
!*                               Sequential Access
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

   type container
      class(base(:)), pointer     :: b1 ! tcx: (:)
      class(base(:)), allocatable :: b2 ! tcx: (:)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp001l
   use m1

   interface write(unformatted)

      subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
         import container
         class(container), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine

   end interface

   ! declaration of variables
   class(container), allocatable  :: b11
   class(container), pointer      :: b12
   type (container)               :: b13
   type (base(3)), target            :: d1 = base(3)('ghi') ! tcx: (3) ! tcx: (3)
   integer :: stat
   character(200) :: msg
   character(9)  :: c1, c2, c3, c4

   ! allocation of variables

   allocate ( b11 )
   allocate ( b12 )

   allocate ( b11%b1, source = base(3)('abc') ) ! tcx: (3)
   allocate ( b11%b2, source = base(3)('def') ) ! tcx: (3)
   allocate ( b12%b1, source = base(3)('ABC') ) ! tcx: (3)
   allocate ( b12%b2, source = base(3)('DEF') ) ! tcx: (3)
   allocate ( b13%b1, source = base(3)('GHI') ) ! tcx: (3)
   allocate ( b13%b2, source = base(3)('JKL') )    ! tcx: (3)

   open (unit = 1, file ='structureComp001l.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b11        !<- write 'abcZdefZY' to file
   write (1, iostat=stat, iomsg=msg )             b12        !<- write 'ABCZDEFZY' to file
   write (1, iostat=stat, iomsg=msg )             b13        !<- write 'GHIZJKLZY' to file
   write (1, iostat=stat, iomsg=msg )             container( b2=base(3)('jkl'), b1=d1 )  !<- write 'ghiZjklZY' to file ! tcx: (3)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1
   read (1, iostat=stat, iomsg=msg )              c2
   read (1, iostat=stat, iomsg=msg )              c3
   read (1, iostat=stat, iomsg=msg )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcZdefZY' )           error stop 101_4
   if ( c2 /= 'ABCZDEFZY' )           error stop 2_4
   if ( c3 /= 'GHIZJKLZY' )           error stop 3_4
   if ( c4 /= 'ghiZjklZY' )           error stop 4_4

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformattedContainer (dtv, unit, iostat, iomsg)
use m1
    class(container), intent(in) :: dtv
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


    write (unit, iostat=iostat, iomsg=iomsg ) dtv%b1, dtv%b2
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
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 14 changes
