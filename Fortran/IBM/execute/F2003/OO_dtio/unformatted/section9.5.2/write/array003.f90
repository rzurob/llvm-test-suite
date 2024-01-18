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
! %GROUP: array003.f
! %VERIFY:
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
!*  DESCRIPTION                : Testing: Section 9.5.2: Data Transfer input/output list
!*                               - Array entity containing unlimited polymorphic component (output)
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
      class(*), pointer :: u
   end type

   type datum
      character(3) :: c = 'ibm'
   end type
end module

program array003
   use m1

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
   class(base), allocatable     :: b1(:)
   class(base), pointer         :: b2(:,:)
   type(base)                   :: b3(2)
   type(base),  pointer         :: b4(:)
   integer(4) :: i1, i2, i3, i4
   character(3) :: c1, c2, c3, c4, c5, c6
   integer :: stat
   character(200) :: msg

   ! allocation of variables
   allocate ( b1(2), b2(2,2) )

   allocate ( b1(1)%u, source = 5 )         !<- integer type
   allocate ( b1(2)%u, source = 'abc' )     !<- character type
   allocate ( b2(1,1)%u, source = datum() ) !<- datum type
   allocate ( b2(2,1)%u, source = 'def' )   !<- character type
   allocate ( b2(1,2)%u, source = 6 )       !<- integer type
   allocate ( b2(2,2)%u, source = 'ghi' )   !<- character type

   b3 = b1
   allocate ( b4(4), source = (/ b3, b3 /) )

   open (unit = 1, file ='array003.data', form='unformatted', access='sequential')

   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg )             b1((/2,1/))
   write (1, iostat=stat, iomsg=msg )             b2
   write (1, iostat=stat, iomsg=msg )             b3
   write (1, iostat=stat, iomsg=msg )             b4(1:4:3)

   rewind 1

   read (1, iostat=stat, iomsg=msg )              c1, i1
   read (1, iostat=stat, iomsg=msg )              c2, c3, i2, c4
   read (1, iostat=stat, iomsg=msg )              i3, c5
   read (1, iostat=stat, iomsg=msg )              i4, c6

   print *, c1, i1
   print *, c2, c3, i2, c4
   print *, i3, c5
   print *, i4, c6

   ! close the file appropriately

   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   interface write(unformatted)
      subroutine writeUnformatteddatum (dtv, unit, iostat, iomsg)
         import datum
         class(datum), intent(in) :: dtv
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine
   end interface

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   associate ( g => dtv%u )
   select type ( g )
      type is (integer)
         write (unit, iostat=iostat, iomsg=iomsg ) g
      type is (character(*))
         write (unit, iostat=iostat, iomsg=iomsg ) g
      type is (datum)
         write (unit, iostat=iostat, iomsg=iomsg ) g
         if ( iomsg /= 'datumwrite' ) error stop 5_4
   end select
   end associate

end subroutine

subroutine writeUnformatteddatum (dtv, unit, iostat, iomsg)
   use m1
   class(datum), intent(in) :: dtv
   integer,  intent(in) :: unit
   integer,  intent(out) :: iostat
   character(*),  intent(inout) :: iomsg

   write ( unit, iostat = iostat ) dtv%c
   iomsg = 'datumwrite'
end subroutine
