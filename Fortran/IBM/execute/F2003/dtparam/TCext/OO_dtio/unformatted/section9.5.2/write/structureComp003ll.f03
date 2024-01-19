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
!*                               - Try output item to be structure component, try parent component
!*                               Direct Access
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

   type, extends(base) :: child
      character(lbase_1) :: cc = ''
   end type

   type container (lcontainer_1) ! lcontainer_1=3
      integer, len :: lcontainer_1
      type(child(lcontainer_1)) :: b1 ! tcx: (lcontainer_1)
      type(child(lcontainer_1)) :: b2 ! tcx: (lcontainer_1)
   end type

contains
   function getC (a)
      class(base(*)), intent(in) :: a ! tcx: (*)
      character(3) :: getC
      getC = a%c
   end function
end module


program structureComp003ll
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
   class(container(:)), allocatable  :: b11 ! tcx: (:)
   class(container(:)), pointer      :: b12 ! tcx: (:)
   type (container(3))               :: b13 ! tcx: (3)
   integer :: stat
   character(200) :: msg
   character(15)  :: c1, c4
   character(7)   :: c2
   character(4)   :: c3

   ! allocation of variables
   allocate ( b11, source = container(3)( b2=child(3)('def','ghi'), b1=child(3)('abc','def') ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   allocate ( b12, source = container(3)( b2=child(3)('DEF','EFG'), b1=child(3)('ABC','DEF') ) ) ! tcx: (3) ! tcx: (3) ! tcx: (3)
   b13 = container(3)( b2=child(3)('JKL','MNO'), b1=child(3)('GHI','JKL') ) ! tcx: (3) ! tcx: (3) ! tcx: (3)

   open (unit = 1, file ='structureComp003ll.data', form='unformatted', access='direct', recl=30)

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg, rec=1 )             b11                !<- write 'abcdefZdefghiZY' to file
   write (1, iostat=stat, iomsg=msg, rec=4 )             b12%b1             !<- write 'ABCDEFZ' to file
   write (1, iostat=stat, iomsg=msg, rec=2 )             b13%b2%base        !<- write 'JKLZ'    to file
   write (1, iostat=stat, iomsg=msg, rec=3 )             container(3)( b2=child(3)('jkl','mno'), b1=child(3)('ghi','jkl') )  !<- write 'ghijklZjklmnoZY' to file ! tcx: (3) ! tcx: (3) ! tcx: (3)

   read (1, iostat=stat, iomsg=msg, rec=1 )              c1
   read (1, iostat=stat, iomsg=msg, rec=4 )              c2
   read (1, iostat=stat, iomsg=msg, rec=2 )              c3
   read (1, iostat=stat, iomsg=msg, rec=3 )              c4

   ! check if the values are set correctly

   if ( c1 /= 'abcdefZdefghiZY' )     error stop 101_4
   if ( c2 /= 'ABCDEFZ' )             error stop 2_4
   if ( c3 /= 'JKLZ' )                error stop 3_4
   if ( c4 /= 'ghijklZjklmnoZY' )     error stop 4_4

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

    select type (dtv)
       type is (child(*)) ! tcx: (*)
          write (unit, iostat=iostat, iomsg=iomsg ) dtv%cc
    end select

    ! add a mark at the end of record, so we know DTIO is used.
    write (unit, iostat=iostat, iomsg=iomsg ) "Z"

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 4 changes
! type: child - added parameters () to invoke with (3) / declare with (*) - 11 changes
! type: container - added parameters (lcontainer_1) to invoke with (3) / declare with (*) - 9 changes
