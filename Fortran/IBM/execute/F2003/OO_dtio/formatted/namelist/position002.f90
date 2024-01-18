!#######################################################################
! SCCS ID Information
! %W%, %I%
! Extract Date/Time: %D% %T%
! Checkin Date/Time: %E% %U%
!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: position002.f
! %VERIFY: position002.1:position002.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE TITLE            :
!*
!*  PROGRAMMER                 : Robert Ma
!*  DATE                       : 11/08/2004
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*                             :
!*  SECONDARY FUNCTIONS TESTED :
!*
!*  DRIVER STANZA              : xlf95
!*
!*  DESCRIPTION                : Testing: Section 10.10 Namelist formatting
!*                                        Try position edit descriptors (T, TL, TR, X) with multiple level of namelist DTIO(Output)
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
   type data
      integer(4) :: k
   end type

   type container
      integer(4) :: j
      type(data), pointer :: d
   end type

   type base
      integer(4) :: i
      type(container), pointer :: c
   end type

   interface write(formatted)
      subroutine writeformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position002
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   namelist /nml/ b1, b2

   allocate( b1, b1%c, b1%c%d )
   allocate( b2, b2%c, b2%c%d )
   
   b1%i = 1
   b1%c%j= 2
   b1%c%d%k= 3
   b2%i = 11
   b2%c%j= 12
   b2%c%d%k= 13

   open (1, file = 'position002.1', form='formatted', access='sequential' )

   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, container
   interface write(formatted)
      subroutine containerwriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import container
         class(container), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(container), allocatable :: c1

   namelist /basedtio/ c1

   allocate ( c1, source = dtv%c )

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "(2X,I4, TL10,'i=',6x)", iostat=iostat)  dtv%i

   if ( iostat /= 0 ) error stop 4_4

   write (unit, basedtio, iostat=iostat, iomsg=iomsg)
   
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'containerdtio' )) error stop 5_4

   iomsg = 'dtiowrite'

end subroutine

subroutine containerwriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: container, data
   
   interface write(formatted)
      subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(in) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(container), intent(in) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   type(data), allocatable :: d1
   namelist /containerdtio/ d1

   allocate (d1, source = dtv%d)
   
   if ( iotype /= "NAMELIST" ) error stop 6_4
   if ( size(v_list, 1) /= 0 ) error stop 7_4

   write (unit, "('i=',I4)", iostat=iostat)  dtv%j

   if ( iostat /= 0 ) error stop 8_4

   write (unit, containerdtio, iostat=iostat, iomsg=iomsg)
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datadtio' )) error stop 9_4
   
   iomsg = 'containerdtio'

end subroutine

subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: data

   class(data), intent(in) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg
   
   if ( iotype /= "NAMELIST" ) error stop 10_4
   if ( size(v_list, 1) /= 0 ) error stop 11_4

   write (unit, "(T1,'i=',T1,TR2,I4)", iostat=iostat )  dtv%k

   iomsg = 'datadtio'

end subroutine
