! *********************************************************************
!*  ===================================================================
!*  XL Fortran Test Case                          IBM INTERNAL USE ONLY
!*  ===================================================================
!*  ===================================================================
!*
!*  TEST CASE NAME             : dummyArgCharctrstc001k
!*
!*  PROGRAMMER                 : David Forster more (derived from dummyArgCharctrstc001 by Robert Ma)
!*  DATE                       : 2007-09-05 (original: 11/08/2004)
!*  ORIGIN                     : AIX Compiler Development, Toronto Lab
!*                             :
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : DTIO
!*  REFERENCE                  : Feature Number 289057(.TCx.dtio) test
!*
!*  DRIVER STANZA              : xlf2003 (original: xlf95)
!*
!*  DESCRIPTION                : Testing: Characteristics of DTIO interface and procedures
!*                               shall be the same as ones defined in Section 9.5.3.7.2.
!*                               1) Dummy Argument Characteristics 
!*                                  -  Same Type
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

program dummyArgCharctrstc001k
   use m1
   
   interface read(unformatted)
      ! read1: defines different type
      subroutine read1 (dtv, unit, iostat, iomsg)
         import base1
         class(base1(4)), intent(inout) :: dtv ! tcx: (4)
         character(1), intent(in) :: unit         !<- shall be integer type
         real, intent(out) :: iostat              !<- shall be integer type
         character(*), intent(inout) :: iomsg
      end subroutine
   end interface
   
   interface read(formatted)    
      ! read2: defines non-default kind type parameter
      subroutine read2 (dtv, unit, iotype, v_list, iostat, iomsg)
         import base1
         class(base1(4)), intent(inout) :: dtv ! tcx: (4)
         integer, intent(in) :: unit
         integer, intent(in) :: iotype            !<- shall be character(*) type
         character(1), intent(in) :: v_list(:)    !<- shall be integer(4) type
         integer, intent(out) :: iostat    
         real, intent(inout) :: iomsg             !<- shall be character(*) type
      end subroutine      
   end interface
   
end program






! Extensions to introduce derived type parameters:
! type: base1 - added parameters (kbase1_1) to invoke with (4) / declare with (4) - 2 changes
