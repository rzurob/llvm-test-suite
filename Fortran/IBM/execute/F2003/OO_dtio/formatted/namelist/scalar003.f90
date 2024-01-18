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
! %GROUP: scalar003.f
! %VERIFY:scalar003.1:scalar003.vf
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
!*                                        Try namelist formatting with polymorphic/nonpoly scalar pointer/allocatable (Output)
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
   type, abstract :: base
      character(3), allocatable :: i
   end type

   type, extends(base) :: child
      character(3), pointer :: i1
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

program scalar003
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base) , pointer     :: b1
   type(child)               :: b3
   type(child) , pointer     :: b4
   character(3), target      :: c1 = 'IBM'

   namelist /nml/ b1, b3, b4

   open (1, file = 'scalar003.1', form='formatted', access='sequential' )
   allocate(child::b1)
   allocate(b4)
   allocate(b1%i, b3%i, b4%i)

   b1%i = 'abc'
   b3%i = 'def'
   b4%i = 'ghi'

   select type ( b1 )
      type is (child)
         b1%i1 => c1
         b3%i1 => b1%i1
         b4%i1 => b3%i1
   end select

   write (1, nml, iostat=stat, iomsg=msg)

   if ( ( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   write (unit, "('i= ',A3,1X)", iostat=iostat )             dtv%i

   select type ( i => dtv )
      class is (child)
         write (unit, "('i1= ',A3,1X)", iostat=iostat )      i%i1
      class default
         error stop 4_4
   end select

   iomsg = 'dtiowrite'

end subroutine
