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
! %GROUP: scalar010.f
! %VERIFY: scalar010.1:scalar010.vf
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
!*                                        Try namelist formatting with polymorphic entities contains polymorphic components (Output)
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

   type basedata
      integer(4) :: i1(2) = (/ -9,-9 /)
   end type

   type, extends(basedata) :: childdata
      integer(4) :: i2(2) = (/ -9,-9 /)
   end type

   type base
      class(basedata), pointer :: bd
   end type

   type, extends(base) :: child
      class(basedata), allocatable :: cd
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

program scalar010
   use m

   integer :: stat
   character(200) :: msg = ''
   class(base), allocatable :: b1
   class(base), pointer     :: b2
   namelist /nml/ b1, b2

   open (1, file = 'scalar010.1', form='formatted', access='sequential' )
   allocate(b1, b1%bd)
   allocate(child :: b2 )
   
   select type(b2)
      type is (child)
         allocate(childdata :: b2%bd, b2%cd)
         select type (g => b2%bd)
            type is (childdata)
               g%i1 = (/123, 234/)
               g%i2 = (/345, 456/)
         end select
         select type (g=>b2%cd)
            type is (childdata)
               g%i1 = (/567, 678/)
               g%i2 = (/789, 890/)
         end select
   end select
  
   b1%bd%i1= (/ 567, 890 /)


   write (1,NML=nml, iostat=stat, iomsg=msg)

   if (( stat /=  0 ) .or. ( msg /= 'dtiowrite' ) ) error stop 1_4

end program


subroutine writeformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: base, child, basedata

   interface write(formatted)
      subroutine datawriteformatted(dtv, unit, iotype, v_list, iostat, iomsg )
         import basedata
         class(basedata), intent(in) :: dtv
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

   class(basedata), allocatable :: dummy1, dummy2

   namelist /basedtio/ dummy1
   namelist /childdtio/ dummy1, dummy2

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (child)
         allocate (dummy1, source = dtv%bd)
         allocate (dummy2, source = dtv%cd)
         write (unit, childdtio, iostat = iostat, iomsg = iomsg)
      class default            !<- if it's not type child, it's type base
         allocate (dummy1, source = dtv%bd)
        write (unit, basedtio, iostat = iostat, iomsg = iomsg)
   end select

   if ( iomsg /= 'datadtio' ) error stop 4_4

   iomsg = 'dtiowrite'

end subroutine

subroutine datawriteformatted (dtv, unit, iotype, v_list, iostat, iomsg)
   use m, only: basedata, childdata

   class(basedata), intent(in) :: dtv
   integer, intent(in) :: unit
   character(*), intent(in) :: iotype
   integer, intent(in)     :: v_list(:)
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   if ( iotype /= "NAMELIST" ) error stop 2_4
   if ( size(v_list, 1) /= 0 ) error stop 3_4

   select type (dtv)
      type is (basedata)
         write (unit, "('basedatacontent:',2(I4) )" , iostat = iostat )   dtv%i1
      type is (childdata)
         write (unit, "('basedatacontent:',2(I4),/)" , iostat = iostat )   dtv%i1
         write (unit, "('childdatacontent:',2(I4),/ )", iostat = iostat )   dtv%i2
   end select

   iomsg = 'datadtio'

end subroutine
