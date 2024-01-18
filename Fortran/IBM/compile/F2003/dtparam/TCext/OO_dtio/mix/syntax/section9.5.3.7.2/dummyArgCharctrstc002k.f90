! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgCharctrstc002k
!*
!*  PROGRAMMER                 : David Forster (derived from dummyArgCharctrstc002 by Robert Ma)
!*  DATE                       : 2007-09-05 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio)
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics 
!*                                  -  Same type parameter
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

   type base1 (kbase1_1) ! kbase1_1=4
      integer, kind :: kbase1_1
      integer(kbase1_1) :: i   
   end type
   
end module

program dummyArgCharctrstc002k
   use m1
   
   interface read(unformatted)
      ! read1: defines different length type parameter
      subroutine read1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit                 
         integer, intent(out) :: iostat              
         character(100), intent(inout) :: iomsg   !<- shall be assumed type parameter
      end subroutine
   end interface
   
   interface write(unformatted)
      ! write1: defines different length and kind type parameters
      subroutine write1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(4)), intent(in) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer(2_4), intent(out) :: iostat      !<- shall be integer(4_4) type
         character(0), intent(inout) :: iomsg     !<- shall be assumed type parameter
      end subroutine
   end interface
   
end program

subroutine read1 (dtv, unit, iostat, iomsg)
   use m1
   class(base1(4)), intent(inout) :: dtv ! tcx: (4)
   integer, intent(in) :: unit                 
   integer, intent(out) :: iostat              
   character(100), intent(inout) :: iomsg
   
   integer(4) temp
   
   read(unit, iostat=iostat, iomsg=iomsg) temp
   dtv%i = temp
end subroutine


subroutine write1 (dtv, unit, iostat, iomsg)
   use m1
   class(base1(4)), intent(in) :: dtv ! tcx: (4)
   integer, intent(in) :: unit
   integer(2_4), intent(out) :: iostat
   character(0), intent(inout) :: iomsg
   
   write (unit, iostat=iostat, iomsg=iomsg) dtv%i
end subroutine


! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 4 changes
