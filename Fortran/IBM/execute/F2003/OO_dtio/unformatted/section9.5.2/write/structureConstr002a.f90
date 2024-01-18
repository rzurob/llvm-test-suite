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
! %GROUP: structureConstr002a.f
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
!*  DESCRIPTION                : Testing: Section 9.5.2 (Data Transfer input/output list)
!*                               - output item is an structure constructor (with type hierarchy, and select type in DTIO)
!*                               Sequential Access
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
   
   type :: base
      character(3) :: c1 = ''
   end type
   
   type, extends(base) :: child
      character(4) :: c2 = ''
   end type

end module


program structureConstr002a
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

   integer :: stat
   character(200) :: msg
   character (8) :: c1
   character (4) :: c2

   ! allocation of variables
   
   open (unit = 1, file ='structureConstr002a.data', form='unformatted', access='sequential')
   
   ! I/O operations
   
   write (1, iostat=stat, iomsg=msg )      child('ibm','nice')
   write (1, iostat=stat, iomsg=msg )      base('xlf')
      
   rewind 1
   
   read (1, iostat=stat, iomsg=msg ) c1
   read (1, iostat=stat, iomsg=msg ) c2

   ! check if the values are set correctly
      
   if ( c1 /= 'ibmniceZ' ) error stop 1_4
   if ( c2 /= 'xlfZ' )     error stop 2_4
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )

end program

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1

   class(base), intent(in) :: dtv
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   select type (dtv)
      type is (base)
         write (unit, iostat=iostat, iomsg=iomsg) dtv
      type is (child)
         write (unit, iostat=iostat, iomsg=iomsg) dtv
   end select
   
   ! add a mark at the end of record, so we know DTIO is used.
   write (unit, iostat=iostat, iomsg=iomsg ) "Z" 
   
end subroutine
