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
! %GROUP: funcRetrn001.f
! %VERIFY: funcRetrn001.1:funcRetrn001.vf
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
!*                                        Try namelist formatting with non polymorphic function return variable
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
      character(3) :: c
      contains
         procedure, pass :: writebase
   end type

   type, extends(base) :: child
      integer(4) :: i
      contains
         procedure, pass :: writechild
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

   integer :: unit = 1
   integer :: stat
   character(200) :: msg

contains

   type(base) function writeBase(dtv)
      class(base), intent(in) :: dtv
      namelist /WB/ writebase
      select type ( dtv )
         type is (base)
            writebase = dtv
      end select

      write (unit, WB, iostat = stat, iomsg = msg )

   end function

   type(child) function writeChild(dtv)
      class(child), intent(in) :: dtv
      namelist /WC/ writeChild
      select type ( dtv )
         type is (child)
            writechild = dtv
      end select

      write (unit, WC, iostat = stat, iomsg = msg )

   end function

end module

program funcRetrn001
   use m

   class(base), allocatable :: b1
   type(base)               :: b2 = base  ('IBM')
   type(child)              :: c1 = child ('FTN',123)
   class(child), pointer    :: c2

   open (1, file = 'funcRetrn001.1', form='formatted', access='sequential' )

   allocate ( b1, source = b2%writebase()  ) !<- writes b2
   allocate ( c2, source = c1%writechild() ) !<- writes c1

   deallocate (b1)
   allocate(b1, source = child('IBM',2005))

   select type ( b1 )
      type is (child)
         c1 = b1%writechild() !<- writes b1
   end select

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base,child

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (base)
         write (unit, "('c=',A3)", iostat=iostat )                  dtv%c
      type is (child)
         write (unit, "('i=',I4,1X,'c=',A3)", iostat=iostat )      dtv%i, &
                                                                   dtv%c
   end select

   iomsg = 'dtiowrite'

end subroutine
