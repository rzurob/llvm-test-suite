! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : C920_001k
!*
!*  PROGRAMMER                 : David Forster (derived from C920_001 by Robert Ma)
!*  DATE                       : 2007-09-10 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Section 9.5 Data transfer statements
!*                               C920: if REC= appears, shall not specify END=, namelist
!*                                     shall not appear, and fmt shall not be *
!*                                     - test REC= with END=
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
   type base (kbase_1) ! kbase_1=4
      integer, kind :: kbase_1
      integer(kbase_1) :: c
   end type
end module

program C920_001k
   use m1
   
   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
      import base
         class (base(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
      import base
         class (base(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(out) :: iostat
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface


   class(base(4)), allocatable :: b1,b2 ! tcx: (4)

   allocate ( b1, source = base(4)(5) ) ! tcx: (4)
   allocate ( b2 )

   open (1, file="C920_001k.udata", recl=5, access="direct", status="replace", form="unformatted" )

   write (1, rec=5) b1

   read (1, rec=5, end=300 ) b2          !<- specify END= with REC= (IBM extension <= allowed!! )
   
300 print *,'IBM extension'

   close ( 1, status = 'delete' )

end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(4)), intent(inout) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    integer(4) :: temp
    read (unit, iostat=iostat, iomsg=iomsg) temp
    dtv%c = temp

end subroutine


subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
    class(base(4)), intent(in) :: dtv ! tcx: (4)
    integer, intent(in) :: unit
    integer, intent(out) :: iostat
    character(*), intent(inout) :: iomsg

    write (unit, iostat=iostat, iomsg=iomsg) dtv%c

end subroutine




! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 6 changes
