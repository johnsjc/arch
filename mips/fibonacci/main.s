## Author: Jc J
## Date: July 29, 2019
##
## Computes the nth fibonacci number using three approaches:
## 1. Recursive solution (with added memoization)
## 2. Tail recursive solution
## 3. Iterative solution
##
## Use fib.sh to run more easily.
##
## Example run:
##
## $ ./fib.sh 7
##
## Input: 7
## [DEBUG] fib(2) already known: 1
## [DEBUG] fib(3) already known: 2
## [DEBUG] fib(4) already known: 3
## [DEBUG] fib(5) already known: 5
##
## Recursive solution: 13
## Tail recursive solution: 13
## Iterative solution: 13
##
.data

input_msg:			.asciiz "Input: "
error_msg:			.asciiz "Input is too large! Enter a value less than 47."
recursive_msg:		.asciiz "\nRecursive solution: "
tail_recursive_msg:	.asciiz "\nTail recursive solution: "
iterative_msg:		.asciiz "\nIterative solution: "

.text

## main
##
## 1. Receives an integer n from stdin.
## 2. Computes the nth fibonacci number using three different algorithms.
##
## Register usage:
##		s0:		n
##
main:
		
	m_read_n:
	
		li			$s7, 1									# global debug flag = true
			
		li			$v0, 5
		syscall	
		move		$s0, $v0								# s0: n
		
		bgt			$s0, 46, error
		
		la			$a0, input_msg							# print "Input: <n>\n"
		li			$v0, 4
		syscall
		move		$a0, $s0
		li			$v0, 1
		syscall
		li			$a0, 0xA
		li			$v0, 11
		syscall
		
	m_recursive:
		
		move		$a0, $s0
		jal			fib.recursion	
		move		$s1, $v0
		
	m_recursive_answer:
		
		la			$a0, recursive_msg
		li			$v0, 4
		syscall
		move		$a0, $s1
		li			$v0, 1
		syscall
		
	m_tail_recursive:
		
		move		$a0, $s0
		jal			fib.tail_recursion	
		move		$s1, $v0
		
	m_tail_recursive_answer:
		
		la			$a0, tail_recursive_msg
		li			$v0, 4
		syscall
		move		$a0, $s1
		li			$v0, 1
		syscall			
		
	m_iterative:
		
		move		$a0, $s0
		jal			fib.iteration
		move		$s1, $v0
		
	m_iterative_answer:
		
		la			$a0, iterative_msg
		li			$v0, 4
		syscall
		move		$a0, $s1
		li			$v0, 1
		syscall			
		
		b exit
		
	error:
	
		la			$a0, error_msg
		li			$v0, 4
		syscall		
		
	exit:
	
		li			$a0, 0xA
		li			$v0, 11
		syscall
		syscall		
	
		li			$v0, 10
		syscall
## end of main

	
.include "fib.s"
.include "debug.s"
