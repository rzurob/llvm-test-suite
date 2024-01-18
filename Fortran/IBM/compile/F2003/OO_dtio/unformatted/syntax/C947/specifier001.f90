!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: redherring.f
! %VERIFY:
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD: dcomp specifier001.f
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DESCRIPTION                : Testing: C947: no specifier should be specified once than once
!*                               Try 5 specifiers including unit=, recl=, exist=, sequential=, and stream=
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
   type base
      character(3) :: c = ''
   end type
end module


program specifier001
   use m1

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   ! declaration of variables
   class(base), allocatable :: b1, b2
   integer :: il, i2
   character(200) :: msg1
   character(3) :: l1, l2

   ! allocation of variables

   allocate (b1,b2)

   b1%c = 'ibm'
   b2%c = ''

   ! I/O operations

   open ( 1, file = 'specifier001.1data', access='sequential', form='unformatted' )
   open ( 2, file = 'specifier001.2data', access='direct'    , form='unformatted', recl=3 )
   open ( 3, file = 'specifier001.3data', access='stream'    , form='unformatted' )


   INQUIRE ( 1, unit = 1, name = msg1 )                           !<- specifying 2 unit=
   INQUIRE ( 2, recl = i1, recl = i2  )                           !<- specifying 2 recl=
   INQUIRE ( 3, stream = l1, stream = l2 )                        !<- specifying 2 stream=

   write ( 1, iostat =i1, iomsg = msg1 )          b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 1_4
   write ( 2, iostat =i1, iomsg = msg1, rec=1 )   b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 2_4
   write ( 3, iostat =i1, iomsg = msg1, pos=1 )   b1
   if ( (i1 /= 0) .or. (msg1 /= 'dtio write'))    error stop 3_4

   rewind 1

   read ( 1, iostat =i1, iomsg = msg1 )           b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'sequential dtio read' ) )       error stop 4_4
   if ( b2%c /= 'ibm' ) error stop 5_4
   b2%c = ''

   read ( 2, iostat =i1, iomsg = msg1, rec=1 )    b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                  error stop 6_4
   if ( b2%c /= 'ibm' ) error stop 7_4
   b2%c = ''

   read ( 3, iostat =i1, iomsg = msg1, pos=1 )    b2
   if (( i1 /= 0 ) .or. ( msg1 /= 'dtio read' ) )                  error stop 8_4
   if ( b2%c /= 'ibm' ) error stop 9_4
   b2%c = ''

   ! close the file appropriately

   close ( 1, status ='delete' )
   close ( 2, status ='delete' )
   close ( 3, status ='delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(inout) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    character(3) :: sequential1

    INQUIRE ( unit, sequential=sequential1, sequential=sequential1 )   !<- specify 2 sequential=

    read (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( sequential1 == 'YES' ) then
       iomsg = 'sequential dtio read'
    else
       iomsg = 'dtio read'
    end if

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base), intent(in) :: dtv
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    logical :: l1

    INQUIRE ( unit, exist = l1, exist = l1 )                     !<- specify 2 exist=

    write (unit, iostat=iostat, iomsg=iomsg ) dtv%c

    if ( l1 )     then
       iomsg = 'dtio write'
    else
       iomsg = 'error'
    end if

end subroutine
