!Copyright (C) 2007 Ashwith Jerome Rego
!
!This program is free software: you can redistribute it and/or modify
!it under the terms of the GNU General Public License as published by
!the Free Software Foundation, either version 3 of the License, or
!(at your option) any later version.
!
!This program is distributed in the hope that it will be useful,
!but WITHOUT ANY WARRANTY; without even the implied warranty of
!MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!GNU General Public License for more details.
!
!You should have received a copy of the GNU General Public License
!along with this program.  If not, see <http://www.gnu.org/licenses/>.


PROGRAM DIVIDEFUNCTION
  IMPLICIT NONE
  INTEGER :: n !degree of polynomial
  REAL, ALLOCATABLE, DIMENSION(:) :: Polynomial, Quotient
  REAL :: h, Remainder
  INTEGER :: i
  
  PRINT*,"Enter degree of polynomial"
  READ*,n
  
  ALLOCATE(Polynomial(n+1))
  ALLOCATE(Quotient(n))
  
  PRINT*,"Enter coefficients. Highest degree terms to lowest:"
  READ*,(Polynomial(i),i=1,n+1)
  
  PRINT*,"Enter value of h (in (x-h), where the polynomial must be divided by (x-h))"
  READ*,h
  
  CALL SDIVIDE(Polynomial, n, h, Quotient, Remainder)
  
  PRINT*,"Quotient(coefficients):"
  PRINT*,(Quotient(i),i=1,n)
  
  PRINT*,"Remainder:"
  PRINT*,Remainder
END PROGRAM DIVIDEFUNCTION


!Author : Louisda16th a.k.a Ashwith J. Rego
!Description: This subroutine divides a polynomial of degree 'n' by a another polynomial of the form x-h
!using synthetic division. 
SUBROUTINE SDIVIDE(f, n, h, q, r)
  IMPLICIT NONE
  
  !Declarations
  !n = degree
  !h = -(coefficient of divisor)
  !f = dividend
  !q = quotient
  !r = reamainder
  INTEGER, INTENT(IN) :: n
  REAL, INTENT(IN) :: h
  REAL,INTENT(IN), DIMENSION(n+1) :: f
  
  REAL, INTENT(INOUT), DIMENSION(n) :: q
  REAL, INTENT(OUT) :: r
  
  INTEGER :: i
  
  !Find quotient
  q(1) = f(1)
  DO i = 2, n
    q(i) = (h*q(i-1)) + f(i)
  END DO
  
  !Find Remainder
  r = h*q(n) + f(n+1)
  
END SUBROUTINE SDIVIDE
