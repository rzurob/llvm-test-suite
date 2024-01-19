!**********************************************************************
!* ====================================================================
!*
!*  DATE                       : 2008-01-29
!*
!*  PRIMARY FUNCTIONS TESTED   : Derived Type Parameters
!*  SECONDARY FUNCTIONS TESTED : Array Constructor Enhancement
!*  REFERENCE                  : Feature Number 289057(.F2003TCx)
!*
!*  DEFECT ABSTRACT            : DTPARAM:ICE: array constructor with kind
!*                               parameters (acTypeLen)
!*
!*  DESCRIPTION                :
!*  <Note by selvanay ("Selvanayagam, Michael (M.K.)"), 2006/08/02 10:52:12,
!*  action: open>
!*
!*  The following test case ICE's in actypelen. The test case uses kind
!*  parameters in a array constructor in an initalization expression.
!*
!*  <Note by mateer (Mateer, Glen), 2008/01/29 12:24:17, seq: 9 rel: 0
!*  action: note>
!*  Found the following variation on the theme:
!*
!*  line 7.22: 1517-005 (U) Error in acTypeLen.  Please contact your Service
!*  Representative.  For more information visit:
!*  http://www.ibm.com/support/docview.wss?uid=swg21110810
!*  1501-511  Compilation failed for file d323718_1.f.
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

module acetint57mod

  type derivedImplied (kderivedImplied_3)
     integer, kind :: kderivedImplied_3
     complex(kderivedImplied_3) ::  &
         zarr(1) = [ complex(4) ::  &
                     CMPLX(1.0,1.0,kderivedImplied_3) ] ! <= Line 7
  end type derivedImplied

end module acetint57mod
