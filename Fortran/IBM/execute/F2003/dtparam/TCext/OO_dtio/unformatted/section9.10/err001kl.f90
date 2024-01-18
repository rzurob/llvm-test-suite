! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : err001kl
!*
!*  PROGRAMMER                 : David Forster (derived from err001 by Robert Ma)
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
!*  DESCRIPTION                : Testing: Section 9.10: Error, end-of-record, and end-of-file conditions
!*                               - use both end and err specifiers with non-end-of-file conditions in a I/O operations,
!*                                 and see if the end branch will be taken correctly
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
      integer(kbase_1) :: c = 0
   end type  
end module


program err001kl
   use m1   

   interface write(unformatted)
      subroutine writeUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4)), intent(in) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface

   interface read(unformatted)
      subroutine readUnformatted (dtv, unit, iostat, iomsg)
         import base
         class(base(4)), intent(inout) :: dtv ! tcx: (4)
         integer,  intent(in) :: unit
         integer,  intent(out) :: iostat
         character(*),  intent(inout) :: iomsg
      end subroutine   
   end interface
  
   ! declaration of variables
   class(base(4)), allocatable :: b1 ! tcx: (4)
   class(base(4)), pointer     :: b2 ! tcx: (4)
   type(base(4)),  allocatable :: b3 ! tcx: (4)
   integer :: stat
   character(200) :: msg
   
   ! allocation of variables
   
   allocate (b1, source = base(4)(1) ) ! tcx: (4)
   allocate (b2, source = base(4)(2) ) ! tcx: (4)
   allocate (b3, source = base(4)(3) ) ! tcx: (4)

   
   open (unit = 1, file ='err001kl.1', form='unformatted', access='sequential')
   open (unit = 3, file ='err001kl.3', form='unformatted', access='stream')   
      
   ! unformatted I/O operations

   write (1, iostat=stat, iomsg=msg)     b1, b2, b3
   write (3, iostat=stat, iomsg=msg)     b2, b1, b3   
   
    read  (1, err=100, end=200, rec=1)   b1               !<- direct access on sequential file
    error stop 101_4

200 error stop 2_4

100 read  (3, err=700, end=800, rec=8)   b1, b2           !<- direct access on sequential file
    error stop 3_4

800 error stop 4_4 
700 continue
   
   ! close the file appropriately
   
   close ( 1, status ='delete' )
   close ( 3, status ='delete' )   
   
end program

subroutine readUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg
    
   read (unit, iomsg=iomsg, iostat=iostat ) dtv%c
   
end subroutine

subroutine writeUnformatted (dtv, unit, iostat, iomsg)
use m1
   class(base(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer, intent(out) :: iostat
   character(*), intent(inout) :: iomsg

   write (unit, iomsg=iomsg, iostat=iostat ) dtv%c

end subroutine

! Extensions to introduce derived type parameters:
! type: base - added parameters (kbase_1) to invoke with (4) / declare with (4) - 10 changes
