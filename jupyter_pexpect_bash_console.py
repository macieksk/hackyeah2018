## Maciek Sykulski 2016-2018
from __future__ import print_function
import sys

if sys.version_info > (3, 0):
    raw_input = input

class BeforePrinter:
    def __init__(self):
        self.before = ''      
    def print_before_diff(self,pbefore):
        if sys.version_info > (3, 0):
            if type(pbefore)==bytes: pbefore=pbefore.decode()
            if type(self.before)==bytes: self.before=self.before.decode()            
        i=pbefore.find(self.before)
        j=self.before.find(pbefore)
        #print("-----")
        #print(list(self.before))
        #print(self.before)
        #print("-----")
        #print(list(pbefore))
        #print(pbefore)
        #print("-----")
        ret=0
        if i<0:
            if j>=0:
                pass
            else:
                ret=len(pbefore)
                print(pbefore,end='')
        else:
            beg = pbefore[:i-1] if i>0 else ''
            ret=len(pbefore)-i-len(self.before)-1
            print(beg+pbefore[i+len(self.before):],end='')
        self.before=pbefore
        return ret

def pexpect_run_with_pass(command="bash",prompt="\$|#",init_pass_prompt=None,max_sigint_count=3,pass_prompt_count=1):
    import pexpect
    import getpass
    p = pexpect.spawn(command,maxread=2000)
    p.setecho(False)
    p.logfile_read=None
    p.logfile_send=None    
    toexpect=([pexpect.TIMEOUT,prompt,init_pass_prompt] if init_pass_prompt else
              [pexpect.TIMEOUT,prompt])
    #print("You only have one try to enter the password, interrupt and restart if failed.")
    beforePrinter=BeforePrinter()
    sigint_count=max_sigint_count    
    prompt_found=False
    while True:
        try:          
            #p.interact()
            i = p.expect(toexpect,timeout=0.1,searchwindowsize=2000)
            prompt_found=prompt_found or i==1
            new=beforePrinter.print_before_diff(p.before)   
            if i==0 or i==1:                                
                try:
                    do_continue=new>0 or not prompt_found
                    if len(p.buffer)>1:
                        new=beforePrinter.print_before_diff(p.buffer)
                        if new>0: do_continue=True
                    rd=p.read_nonblocking(size=100000, timeout=0)                                          
                except pexpect.TIMEOUT: 
                    if do_continue: continue
                sigint_count=0
                inp=raw_input(prompt+" ")                
                p.sendline(inp)                
                beforePrinter.before=inp+"\r\n"
                sigint_count=max_sigint_count
                prompt_found=False
            elif i==2:
                p.sendline(getpass.getpass(init_pass_prompt+' '))
                pass_prompt_count-=1
                if pass_prompt_count<=0:
                    toexpect=[pexpect.TIMEOUT, prompt]                
        except pexpect.EOF:
            break
        except KeyboardInterrupt:
            p.sendintr()
            sigint_count-=1
            if sigint_count<0:
                break

#RCFILE='<(echo \'alias ll="\'"\'"ls --color=always"\'"\'"\')'
RCFILE='/dev/null' #alias ls='ls --color=always\''
def pexpect_sudo_bash(dir=".",rcstring=RCFILE):
    return pexpect_run_with_pass(command="sudo bash -l -i -c 'cd {} && PS1=\"$PS1\"\"--pexpect--$\" bash --norc --rcfile {}'".format(dir,rcstring),
                                 prompt="--pexpect--\$",init_pass_prompt=":")

def pexpect_ssh_sudo_bash(dir=".",connection="user@localhost",rcstring=RCFILE):
    return pexpect_run_with_pass(command="ssh -t {} \"sudo bash -l -i -c 'cd {} && PS1=\"$PS1\"\"--pexpect--$\" bash --norc --rcfile {}'\"".format(connection,dir,rcstring),
                                 prompt="--pexpect--\$",init_pass_prompt=":",pass_prompt_count=2)
#pexpect_su_sudo_bash = lambda: pexpect_run_with_pass(command="su user -c sudo bash",prompt="\$|#|:")

def pexpect_bash(dir='.',rcstring=RCFILE):
    return pexpect_run_with_pass(command="bash -l -i -c 'cd {} && PS1=\"$PS1\"\"--pexpect--$\" bash --norc --rcfile {}'".format(dir,rcstring),                                                              
                                 #"bash -li -c 'bash -li <<< \"cd {} && export PS1=\"$PS1\"'--pexpect--$' && exec </dev/tty\"'".format(dir),    
                                 prompt="--pexpect--\$",init_pass_prompt=None)
                             
