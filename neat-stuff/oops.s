
a.out:     file format elf64-x86-64


Disassembly of section .init:

00000000004003f0 <_init>:
  4003f0:	48 83 ec 08          	sub    $0x8,%rsp
  4003f4:	48 8b 05 fd 0b 20 00 	mov    0x200bfd(%rip),%rax        # 600ff8 <_DYNAMIC+0x1d0>
  4003fb:	48 85 c0             	test   %rax,%rax
  4003fe:	74 05                	je     400405 <_init+0x15>
  400400:	e8 3b 00 00 00       	callq  400440 <__libc_start_main@plt+0x10>
  400405:	48 83 c4 08          	add    $0x8,%rsp
  400409:	c3                   	retq   

Disassembly of section .plt:

0000000000400410 <__stack_chk_fail@plt-0x10>:
  400410:	ff 35 f2 0b 20 00    	pushq  0x200bf2(%rip)        # 601008 <_GLOBAL_OFFSET_TABLE_+0x8>
  400416:	ff 25 f4 0b 20 00    	jmpq   *0x200bf4(%rip)        # 601010 <_GLOBAL_OFFSET_TABLE_+0x10>
  40041c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400420 <__stack_chk_fail@plt>:
  400420:	ff 25 f2 0b 20 00    	jmpq   *0x200bf2(%rip)        # 601018 <_GLOBAL_OFFSET_TABLE_+0x18>
  400426:	68 00 00 00 00       	pushq  $0x0
  40042b:	e9 e0 ff ff ff       	jmpq   400410 <_init+0x20>

0000000000400430 <__libc_start_main@plt>:
  400430:	ff 25 ea 0b 20 00    	jmpq   *0x200bea(%rip)        # 601020 <_GLOBAL_OFFSET_TABLE_+0x20>
  400436:	68 01 00 00 00       	pushq  $0x1
  40043b:	e9 d0 ff ff ff       	jmpq   400410 <_init+0x20>

Disassembly of section .plt.got:

0000000000400440 <.plt.got>:
  400440:	ff 25 b2 0b 20 00    	jmpq   *0x200bb2(%rip)        # 600ff8 <_DYNAMIC+0x1d0>
  400446:	66 90                	xchg   %ax,%ax

Disassembly of section .text:

0000000000400450 <_start>:
  400450:	31 ed                	xor    %ebp,%ebp
  400452:	49 89 d1             	mov    %rdx,%r9
  400455:	5e                   	pop    %rsi
  400456:	48 89 e2             	mov    %rsp,%rdx
  400459:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  40045d:	50                   	push   %rax
  40045e:	54                   	push   %rsp
  40045f:	49 c7 c0 a0 06 40 00 	mov    $0x4006a0,%r8
  400466:	48 c7 c1 30 06 40 00 	mov    $0x400630,%rcx
  40046d:	48 c7 c7 46 05 40 00 	mov    $0x400546,%rdi
  400474:	e8 b7 ff ff ff       	callq  400430 <__libc_start_main@plt>
  400479:	f4                   	hlt    
  40047a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400480 <deregister_tm_clones>:
  400480:	b8 3f 10 60 00       	mov    $0x60103f,%eax
  400485:	55                   	push   %rbp
  400486:	48 2d 38 10 60 00    	sub    $0x601038,%rax
  40048c:	48 83 f8 0e          	cmp    $0xe,%rax
  400490:	48 89 e5             	mov    %rsp,%rbp
  400493:	76 1b                	jbe    4004b0 <deregister_tm_clones+0x30>
  400495:	b8 00 00 00 00       	mov    $0x0,%eax
  40049a:	48 85 c0             	test   %rax,%rax
  40049d:	74 11                	je     4004b0 <deregister_tm_clones+0x30>
  40049f:	5d                   	pop    %rbp
  4004a0:	bf 38 10 60 00       	mov    $0x601038,%edi
  4004a5:	ff e0                	jmpq   *%rax
  4004a7:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  4004ae:	00 00 
  4004b0:	5d                   	pop    %rbp
  4004b1:	c3                   	retq   
  4004b2:	0f 1f 40 00          	nopl   0x0(%rax)
  4004b6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4004bd:	00 00 00 

00000000004004c0 <register_tm_clones>:
  4004c0:	be 38 10 60 00       	mov    $0x601038,%esi
  4004c5:	55                   	push   %rbp
  4004c6:	48 81 ee 38 10 60 00 	sub    $0x601038,%rsi
  4004cd:	48 c1 fe 03          	sar    $0x3,%rsi
  4004d1:	48 89 e5             	mov    %rsp,%rbp
  4004d4:	48 89 f0             	mov    %rsi,%rax
  4004d7:	48 c1 e8 3f          	shr    $0x3f,%rax
  4004db:	48 01 c6             	add    %rax,%rsi
  4004de:	48 d1 fe             	sar    %rsi
  4004e1:	74 15                	je     4004f8 <register_tm_clones+0x38>
  4004e3:	b8 00 00 00 00       	mov    $0x0,%eax
  4004e8:	48 85 c0             	test   %rax,%rax
  4004eb:	74 0b                	je     4004f8 <register_tm_clones+0x38>
  4004ed:	5d                   	pop    %rbp
  4004ee:	bf 38 10 60 00       	mov    $0x601038,%edi
  4004f3:	ff e0                	jmpq   *%rax
  4004f5:	0f 1f 00             	nopl   (%rax)
  4004f8:	5d                   	pop    %rbp
  4004f9:	c3                   	retq   
  4004fa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000400500 <__do_global_dtors_aux>:
  400500:	80 3d 31 0b 20 00 00 	cmpb   $0x0,0x200b31(%rip)        # 601038 <__TMC_END__>
  400507:	75 11                	jne    40051a <__do_global_dtors_aux+0x1a>
  400509:	55                   	push   %rbp
  40050a:	48 89 e5             	mov    %rsp,%rbp
  40050d:	e8 6e ff ff ff       	callq  400480 <deregister_tm_clones>
  400512:	5d                   	pop    %rbp
  400513:	c6 05 1e 0b 20 00 01 	movb   $0x1,0x200b1e(%rip)        # 601038 <__TMC_END__>
  40051a:	f3 c3                	repz retq 
  40051c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000400520 <frame_dummy>:
  400520:	bf 20 0e 60 00       	mov    $0x600e20,%edi
  400525:	48 83 3f 00          	cmpq   $0x0,(%rdi)
  400529:	75 05                	jne    400530 <frame_dummy+0x10>
  40052b:	eb 93                	jmp    4004c0 <register_tm_clones>
  40052d:	0f 1f 00             	nopl   (%rax)
  400530:	b8 00 00 00 00       	mov    $0x0,%eax
  400535:	48 85 c0             	test   %rax,%rax
  400538:	74 f1                	je     40052b <frame_dummy+0xb>
  40053a:	55                   	push   %rbp
  40053b:	48 89 e5             	mov    %rsp,%rbp
  40053e:	ff d0                	callq  *%rax
  400540:	5d                   	pop    %rbp
  400541:	e9 7a ff ff ff       	jmpq   4004c0 <register_tm_clones>

0000000000400546 <main>:
// Making a VERY fast dfa in c using computed goto's

int main() {
  400546:	55                   	push   %rbp
  400547:	48 89 e5             	mov    %rsp,%rbp
  40054a:	48 83 ec 30          	sub    $0x30,%rsp
  40054e:	64 48 8b 04 25 28 00 	mov    %fs:0x28,%rax
  400555:	00 00 
  400557:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  40055b:	31 c0                	xor    %eax,%eax
    int input_string[] = {0, 1, 1, 0, 1};
  40055d:	c7 45 e0 00 00 00 00 	movl   $0x0,-0x20(%rbp)
  400564:	c7 45 e4 01 00 00 00 	movl   $0x1,-0x1c(%rbp)
  40056b:	c7 45 e8 01 00 00 00 	movl   $0x1,-0x18(%rbp)
  400572:	c7 45 ec 00 00 00 00 	movl   $0x0,-0x14(%rbp)
  400579:	c7 45 f0 01 00 00 00 	movl   $0x1,-0x10(%rbp)
    int len = 5;
  400580:	c7 45 dc 05 00 00 00 	movl   $0x5,-0x24(%rbp)
    int i = 0;
  400587:	c7 45 d8 00 00 00 00 	movl   $0x0,-0x28(%rbp)
    goto state_1;
  40058e:	90                   	nop
    state_1:
    if (i == len) return 1;
  40058f:	8b 45 d8             	mov    -0x28(%rbp),%eax
  400592:	3b 45 dc             	cmp    -0x24(%rbp),%eax
  400595:	75 07                	jne    40059e <main+0x58>
  400597:	b8 01 00 00 00       	mov    $0x1,%eax
  40059c:	eb 72                	jmp    400610 <main+0xca>
    goto *(&&state_1_start + input_string[i++]);
  40059e:	8b 45 d8             	mov    -0x28(%rbp),%eax
  4005a1:	8d 50 01             	lea    0x1(%rax),%edx
  4005a4:	89 55 d8             	mov    %edx,-0x28(%rbp)
  4005a7:	48 98                	cltq   
  4005a9:	8b 44 85 e0          	mov    -0x20(%rbp,%rax,4),%eax
  4005ad:	48 98                	cltq   
  4005af:	48 05 b7 05 40 00    	add    $0x4005b7,%rax
  4005b5:	eb 02                	jmp    4005b9 <main+0x73>
    state_1_start:
    goto state_2;
  4005b7:	eb 02                	jmp    4005bb <main+0x75>
  4005b9:	ff e0                	jmpq   *%rax
    goto state_3;

    state_2: 
    if (i == len) return 2;
  4005bb:	8b 45 d8             	mov    -0x28(%rbp),%eax
  4005be:	3b 45 dc             	cmp    -0x24(%rbp),%eax
  4005c1:	75 07                	jne    4005ca <main+0x84>
  4005c3:	b8 02 00 00 00       	mov    $0x2,%eax
  4005c8:	eb 46                	jmp    400610 <main+0xca>
    goto *(&&state_2_start + input_string[i++]);
  4005ca:	8b 45 d8             	mov    -0x28(%rbp),%eax
  4005cd:	8d 50 01             	lea    0x1(%rax),%edx
  4005d0:	89 55 d8             	mov    %edx,-0x28(%rbp)
  4005d3:	48 98                	cltq   
  4005d5:	8b 44 85 e0          	mov    -0x20(%rbp,%rax,4),%eax
  4005d9:	48 98                	cltq   
  4005db:	48 05 e3 05 40 00    	add    $0x4005e3,%rax
  4005e1:	eb d6                	jmp    4005b9 <main+0x73>
    state_2_start:
    goto state_2;
  4005e3:	eb d6                	jmp    4005bb <main+0x75>
    goto state_3;

    state_3:
    if (i == len) return 3;
  4005e5:	b8 03 00 00 00       	mov    $0x3,%eax
  4005ea:	eb 24                	jmp    400610 <main+0xca>
    goto *(&&state_3_start + input_string[i++]);
  4005ec:	8b 45 d8             	mov    -0x28(%rbp),%eax
  4005ef:	8d 50 01             	lea    0x1(%rax),%edx
  4005f2:	89 55 d8             	mov    %edx,-0x28(%rbp)
  4005f5:	48 98                	cltq   
  4005f7:	8b 44 85 e0          	mov    -0x20(%rbp,%rax,4),%eax
  4005fb:	48 98                	cltq   
  4005fd:	48 05 05 06 40 00    	add    $0x400605,%rax
  400603:	eb b4                	jmp    4005b9 <main+0x73>
    state_3_start:
    goto state_3;
  400605:	90                   	nop
    state_2_start:
    goto state_2;
    goto state_3;

    state_3:
    if (i == len) return 3;
  400606:	8b 45 d8             	mov    -0x28(%rbp),%eax
  400609:	3b 45 dc             	cmp    -0x24(%rbp),%eax
  40060c:	75 de                	jne    4005ec <main+0xa6>
  40060e:	eb d5                	jmp    4005e5 <main+0x9f>
    goto *(&&state_3_start + input_string[i++]);
    state_3_start:
    goto state_3;
    goto state_2;
}
  400610:	48 8b 4d f8          	mov    -0x8(%rbp),%rcx
  400614:	64 48 33 0c 25 28 00 	xor    %fs:0x28,%rcx
  40061b:	00 00 
  40061d:	74 05                	je     400624 <main+0xde>
  40061f:	e8 fc fd ff ff       	callq  400420 <__stack_chk_fail@plt>
  400624:	c9                   	leaveq 
  400625:	c3                   	retq   
  400626:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40062d:	00 00 00 

0000000000400630 <__libc_csu_init>:
  400630:	41 57                	push   %r15
  400632:	41 56                	push   %r14
  400634:	41 89 ff             	mov    %edi,%r15d
  400637:	41 55                	push   %r13
  400639:	41 54                	push   %r12
  40063b:	4c 8d 25 ce 07 20 00 	lea    0x2007ce(%rip),%r12        # 600e10 <__frame_dummy_init_array_entry>
  400642:	55                   	push   %rbp
  400643:	48 8d 2d ce 07 20 00 	lea    0x2007ce(%rip),%rbp        # 600e18 <__init_array_end>
  40064a:	53                   	push   %rbx
  40064b:	49 89 f6             	mov    %rsi,%r14
  40064e:	49 89 d5             	mov    %rdx,%r13
  400651:	4c 29 e5             	sub    %r12,%rbp
  400654:	48 83 ec 08          	sub    $0x8,%rsp
  400658:	48 c1 fd 03          	sar    $0x3,%rbp
  40065c:	e8 8f fd ff ff       	callq  4003f0 <_init>
  400661:	48 85 ed             	test   %rbp,%rbp
  400664:	74 20                	je     400686 <__libc_csu_init+0x56>
  400666:	31 db                	xor    %ebx,%ebx
  400668:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  40066f:	00 
  400670:	4c 89 ea             	mov    %r13,%rdx
  400673:	4c 89 f6             	mov    %r14,%rsi
  400676:	44 89 ff             	mov    %r15d,%edi
  400679:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  40067d:	48 83 c3 01          	add    $0x1,%rbx
  400681:	48 39 eb             	cmp    %rbp,%rbx
  400684:	75 ea                	jne    400670 <__libc_csu_init+0x40>
  400686:	48 83 c4 08          	add    $0x8,%rsp
  40068a:	5b                   	pop    %rbx
  40068b:	5d                   	pop    %rbp
  40068c:	41 5c                	pop    %r12
  40068e:	41 5d                	pop    %r13
  400690:	41 5e                	pop    %r14
  400692:	41 5f                	pop    %r15
  400694:	c3                   	retq   
  400695:	90                   	nop
  400696:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40069d:	00 00 00 

00000000004006a0 <__libc_csu_fini>:
  4006a0:	f3 c3                	repz retq 

Disassembly of section .fini:

00000000004006a4 <_fini>:
  4006a4:	48 83 ec 08          	sub    $0x8,%rsp
  4006a8:	48 83 c4 08          	add    $0x8,%rsp
  4006ac:	c3                   	retq   
