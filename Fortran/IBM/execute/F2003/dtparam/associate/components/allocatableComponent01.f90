!***********************************************************************
!* =====================================================================
!*
!*  TEST CASE NAME             : allocatableComponent01
!*                               an ALLOCATABLE Component
!*
!*  DATE                       : August 15, 2008
!*  ORIGIN                     : AIX Compiler Development,
!*
!*  PRIMARY FUNCTIONS TESTED   : selector is a variable
!*  SECONDARY FUNCTIONS TESTED : The Variable has a Component with the
!*                               ALLOCATABLE Attribute
!*
!*  REQUIRED COMPILER OPTIONS  :
!*
!*  KEYWORD(S)                 : ASSOCIATE
!*  TARGET(S)                  :
!*  NUMBER OF TESTS CONDITIONS :
!*
!*  DESCRIPTION                :
!*  Using the Basic Testing (as defined above), where selector:
!*  * Is a Derived Type with:
!*    o An ALLOCATABLE Component
!*
!23456789012345678901234567890123456789012345678901234567890123456789012

MODULE char
    IMPLICIT NONE

    TYPE characters(l1,l2)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        CHARACTER(l1) :: set( l2 )
    END TYPE characters

END MODULE char


MODULE contain
    USE char

    IMPLICIT NONE

    TYPE container(l1,l2)
        INTEGER, LEN :: l1
        INTEGER, LEN :: l2

        TYPE(characters(l1,l2)), ALLOCATABLE :: char
    END TYPE container

END MODULE contain


PROGRAM allocatableComponent01
    USE contain

    IMPLICIT NONE

    INTEGER :: i

    CHARACTER(1) :: upperCaseLetters( 26 ) = [ (CHAR( i ), i = 65, 90) ]
    CHARACTER(5) :: fiveLetterWords( 2 ) = [ 'Hello', 'World' ]

    TYPE(container(1,26)) :: alphabet


    alphabet = container(1,26)(characters(1,26)(upperCaseLetters))

    ASSOCIATE(abc => alphabet)

        IF (LEN( abc%char%set ) /= LEN( upperCaseLetters )) THEN
            STOP 10

        ELSE IF (abc%char%l1 /= LEN( upperCaseLetters )) THEN
            STOP 11

        ELSE IF (abc%l1 /= LEN( upperCaseLetters )) THEN
            STOP 12

        ELSE IF (SIZE( abc%char%set ) /= SIZE( upperCaseLetters )) THEN
            STOP 13

        ELSE IF (abc%char%l2 /= SIZE( upperCaseLetters )) THEN
            STOP 14

        ELSE IF (abc%l2 /= SIZE( upperCaseLetters )) THEN
            STOP 15

        ELSE IF ( ANY(abc%char%set /= upperCaseLetters) ) THEN
            STOP 16
        END IF

    END ASSOCIATE

    call foo (container(5,2)(characters(5,2)(fiveLetterWords)))

    contains

!    ASSOCIATE(m => container(5,2)(characters(5,2)(fiveLetterWords)))
    subroutine foo (m)
        type(container(*,*)), intent(in) :: m

        IF (LEN( m%char%set ) /= LEN( fiveLetterWords )) THEN
            STOP 20

        ELSE IF (m%char%l1 /= LEN( fiveLetterWords )) THEN
            STOP 21

        ELSE IF (m%l1 /= LEN( fiveLetterWords )) THEN
            STOP 22

        ELSE IF (SIZE( m%char%set ) /= SIZE( fiveLetterWords )) THEN
            STOP 23

        ELSE IF (m%char%l2 /= SIZE( fiveLetterWords )) THEN
            STOP 24

        ELSE IF (m%l2 /= SIZE( fiveLetterWords )) THEN
            STOP 25

        ELSE IF ( ANY(m%char%set /= fiveLetterWords) ) THEN
            STOP 26
        END IF

    END subroutine

END PROGRAM allocatableComponent01
