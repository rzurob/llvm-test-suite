! *********************************************************************
!*  ===================================================================
!*
!*  TEST CASE NAME             : specifier001l
!*
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DESCRIPTION                : Testing: C948: An inquire-spec-list shall contain one FILE= specifier or one UNIT= specifier
!*                                        but not both.
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
end module


program specifier001l
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(inout) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(*)), intent(in) :: dtv ! tcx: (*)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base(:)), allocatable :: b1, b2 ! tcx: (:)
   character(200) :: msg1
   integer :: stat1
   character(10)  :: char1

   ! allocation of variables

   allocate (base(3)::b1,b2) ! tcx: base(3)

   b1%c = 'ibm'
   b2%c = ''

   ! I/O operations

   open ( 2, file = 'specifier001l.2data', access='direct', form='unformatted', recl=3 )

   INQUIRE ( 2, file = 'specifier001l.2data', access=char1 )                         !<- specify both unit= and file=

   write (2, iostat=stat1, iomsg=msg1, rec = 4 )    b1

   read  (2, iostat=stat1, iomsg=msg1, rec = 4 )    b2


   ! close the file appropriately

   close ( 2, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(inout) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: sequential1

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 )    call zzrc()

    INQUIRE ( iostat=iostat, iomsg=iomsg, sequential=sequential1 )  !<- no unit or file specified

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(*)), intent(in) :: dtv ! tcx: (*)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(10) :: fileName = ''
    character(10) :: accessName = ''
    character(10) :: formName = ''

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( iostat /= 0 )    call zzrc()

    INQUIRE ( unit, name = fileName )

    INQUIRE ( unit, file = fileName, access=accessName )        !<- unit= and file= specified together

    INQUIRE ( iostat=iostat, iomsg=iomsg, form=formName )       !<- no unit specified

end subroutine


! Extensions to introduce derived type parameters:
! type: base - added parameters (lbase_1) to invoke with (3) / declare with (*) - 5 changes
