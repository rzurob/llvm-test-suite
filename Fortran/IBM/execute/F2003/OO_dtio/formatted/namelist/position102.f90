!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: position102.f
! %VERIFY: position102.1:position102.vf
! %STDIN:
! %STDOUT:
! %EXECARGS:
! %POSTCMD:
! %END
! *********************************************************************
!*  ===================================================================
!*
!*  DATE                       : 11/08/2004
!*
!*  PRIMARY FUNCTIONS TESTED   :
!*  SECONDARY FUNCTIONS TESTED :
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

   interface read(formatted)
      subroutine readformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import base
         class(base), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

end module

program position102
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   namelist /nml/ b1, b2

   allocate( b1, b2)   !b1%c, b1%c%d )
   allocate( b1%c, b1%c%d, b2%c, b2%c%d )

   b1%i = 1
   b1%c%j= 2
   b1%c%d%k= 3
   b2%i = 11
   b2%c%j= 12
   b2%c%d%k= 13

   open (1, file = 'position102.1', form='formatted', access='sequential' )

   read (1,NML=nml, iostat=stat, iomsg=msg)
   if (( stat /=  0 ) .or. ( msg /= 'dtioread' ) ) error stop 1_4

   if ( ( b1%i /= 1001 ) .or. ( b1%c%j /= 1002 ) .or. ( b1%c%d%k /= 1003 ) ) error stop 2_4
   if ( ( b2%i /= 2001 ) .or. ( b2%c%j /= 2002 ) .or. ( b2%c%d%k /= 2003 ) ) error stop 3_4

end program


subroutine readformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, container
   interface read(formatted)
      subroutine containerreadformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import container
         class(container), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(inout) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(container), allocatable :: c1
   namelist /basedtio/ c1
   allocate(c1)
   nullify (c1%d)

   if ( iotype /= "NAMELIST" ) error stop 4_4
   if ( size(v_list, 1) /= 0 ) error stop 5_4

   read (unit, "(TL100, I4)", iostat=iostat, iomsg=iomsg)  dtv%i

   if ( iostat /= 0 ) error stop 6_4

   read (unit, basedtio, iostat=iostat, iomsg=iomsg)

   if ( associated(dtv%c) ) then
      deallocate (dtv%c)
   end if

   allocate (dtv%c, source = c1)

   if ( ( iostat /= 0 ) .or. ( iomsg /= 'containerdtio' )) error stop 7_4

   iomsg = 'dtioread'

end subroutine

subroutine containerreadformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: container, data

   interface read(formatted)
      subroutine datareadformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import data
         class(data), intent(inout) :: dtv
         integer,  intent(in) :: unit
         character(*), intent(in) :: iotype
         integer, intent(in)     :: v_list(:)
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(container), intent(inout) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   type(data), allocatable :: d1
   namelist /containerdtio/ d1
   allocate(d1)

   if ( iotype /= "NAMELIST" ) error stop 8_4
   if ( size(v_list, 1) /= 0 ) error stop 9_4

   read (unit, "(TR10, T1,I4)", iostat=iostat)  dtv%j

   if ( iostat /= 0 ) error stop 10_4

   read (unit, containerdtio, iostat=iostat, iomsg=iomsg)
   if ( ( iostat /= 0 ) .or. ( iomsg /= 'datadtio' )) error stop 11_4

   if ( associated ( dtv%d ) ) then
      deallocate (dtv%d)
   end if

   allocate (dtv%d, source = d1)
   iomsg = 'containerdtio'

end subroutine

subroutine datareadformatted(dtv, unit, iotype, v_list, iostat, iomsg )
   use m, only: data

   class(data), intent(inout) :: dtv
   integer,  intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 12_4
   if ( size(v_list, 1) /= 0 ) error stop 13_4

   read (unit, "(5X, TL99, I4)", iostat=iostat )  dtv%k

   iomsg = 'datadtio'

end subroutine
