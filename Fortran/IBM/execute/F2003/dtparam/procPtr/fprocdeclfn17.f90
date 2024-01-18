!=======================================================================
! TEST BUCKET                : F2003/dtparam/procPtr/
! DATE                       : 07/31/2008
! PRIMARY FUNCTIONS TESTED   : procedure declaration statement & procedure component
! DESCRIPTION                : Use of  procedure declaration statement with Interface declared in another module
!
!=======================================================================
! REVISION HISTORY
!                   MM/DD/YY :
!                       Init :
!                   Comments :
!=======================================================================
!23456789012345678901234567890123456789012345678901234567890123456789012

program procdeclfn17
 USE FunInterface
 USE FunImplemention
 USE TypeDef

 PROCEDURE(ICREATE_LIST),pointer :: procptr1=>NULL()
 PROCEDURE(IPRINT_LIST),pointer :: procptr2=>NULL()
 PROCEDURE(IINIT_List),pointer :: procptr3=>NULL()
 PROCEDURE(ICSHIFT_ARRAYS),pointer :: procptr4=>NULL()
 INTEGER(KIND=4)                 :: procptrRC=0,NumberofListItem=5
  TYPE(Node),POINTER :: LastItem,CurrentItem

 procptr1=>CREATE_LIST
 procptr2=>PRINT_LIST
 procptr3=>INIT_List
 procptr4=>CSHIFT_ARRAYS

 print*,"Initial List"
 procptrRC=procptr1(LastItem,CurrentItem,NumberofListItem)
 print*,"Populating the List"
 procptrRC=procptr3(LastItem,CurrentItem,NumberofListItem)
 print*,"Print the List"
 procptrRC=procptr2(LastItem,CurrentItem,NumberofListItem) ! print in order
 print*,"CSHIFT our List"
 procptrRC=procptr4(LastItem,CurrentItem,NumberofListItem)
end program procdeclfn17