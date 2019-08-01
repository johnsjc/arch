## Author: Jc J
## Date: July 29, 2019
##
## Debug output for fibonacci program
##

.globl debug.memo

.data

debug_memo1:			.asciiz "[DEBUG] fib("
debug_memo2:			.asciiz ") already known: "

.text

## memo
##
## Debug information for memoization
##
## Arguments
##		a0:		n
##		a1:		memo[n]
##
debug.memo:

		beqz		$s7, __m_end                # skip if debug flag not set

		move		$t0, $a0					# preserve n and memo[n]
		move		$t1, $a1

		la			$a0, debug_memo1			# "fib("
		li			$v0, 4
		syscall
		
		move		$a0, $t0					# n
		li			$v0, 1
		syscall
		
		la			$a0, debug_memo2			#") already known: "
		li			$v0, 4
		syscall
		
		move		$a0, $t1					# memo[n]
		li			$v0, 1
		syscall
		
		li			$a0, 0xA					# new line
		li			$v0, 11
		syscall		
		
	__m_end:
		
		jr			$ra
