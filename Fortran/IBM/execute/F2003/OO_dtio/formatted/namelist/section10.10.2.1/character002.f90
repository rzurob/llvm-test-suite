!#######################################################################
! *********************************************************************
! %START
! %MAIN: YES
! %PRECMD: rm -f *.mod
! %COMPOPTS: -qfree=f90
! %GROUP: character002.f
! %VERIFY: character002.1:character002.vf
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
!*                                        Character output with namelist formatting on external files
!*                                        character sequences are not separated by value separators
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

   type base
      character(2) :: c1(3)
   end type

   type, extends(base) :: child
      character(2) :: c2(3)
   end type

end module

module m1
   use m
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

program character002
   use m1

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable  :: b1
   class(base), pointer      :: b2

   namelist /nml1/ b1, b2

   open (1, file='character002.1', form='formatted', access='sequential', blank='zero' )

   allocate( b1, source = child((/'ab','cd','ef'/),(/'AB','CD','EF'/)) )
   allocate( b2, source = child((/'gh','ij','kl'/),(/'GH','IJ','KL'/)) )

   write (1, NML=nml1, iostat=stat, iomsg=msg)
   if ( ( stat /= 0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   type(base)  :: dumb
   type(child) :: dumc

   namelist /b/  dumb
   namelist /c/  dumc

   if ( iotype /= "NAMELIST" ) error stop 9_4
   if ( size(v_list, 1) /= 0 ) error stop 10_4

   select type ( dtv )
      type is (base)
         dumb=dtv
         write ( unit, b, iostat = iostat )
      type is (child)
         dumc=dtv
         write ( unit, c, iostat = iostat )
   end select

   iomsg = 'dtiowrite'

end subroutine
