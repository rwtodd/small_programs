import copy

def compute_gematria(w): 
  return sum(ord(i) - ord('a') + 1 for i in w)

class word:
  def __init__(self, w):
    self.__word = ''.join([l for l in w.lower() if l.isalpha()])
    self.__value = compute_gematria(self.__word)
    self.__uses = 0
  def str(self):  return self.__word
  def val(self):  return self.__value
  def uses(self):  return self.__uses
  def incr(self): self.__uses = self.__uses + 1 
  def __eq__(self,b): return (self.__word == b.__word)

class dictionary:
  def __init__(self):
    self.__words = {}
  def add(self,w):
    wrds = self.__words.setdefault(w.val(),[w])
    if(w in wrds):
       wrds[wrds.index(w)].incr()
    else:
       w.incr()
       wrds.append(w)
  def cluster(self,num):  
      return copy.deepcopy(self.__words[num])
  def numbers(self):
      return self.__words.keys() 
  def words(self):
      ans = []
      for wlist in self.__words.values():
        ans.extend( copy.deepcopy(wlist) )
      return ans

def display_clusters(d):
  for n in d.numbers():
    cl = [ i.str() for i in d.cluster(n) ]
    if(len(cl) > 1):
       print(n,': ',cl,sep='')

def display_common_words(d,num):
  for w in sorted(d.words(), key=lambda l: l.uses())[-num:]:
    print(w.str(),': (',w.uses(),')',sep='')

def display_word_values(d):
  for w in sorted(d.words(),key=lambda l: l.str()):
    print(w.str(),': (',w.val(),')',sep='')

def main():
  theDict = dictionary()
  fl = open('fl.txt',"r")
  whole_thing = fl.read().split()
  fl.close()
  for w in (word(str) for str in whole_thing):
    if w.val() > 0:  theDict.add(w)
  print("Clusters ~~~~~~")
  display_clusters(theDict)
  print("\n\nMost common words ~~~~~")
  display_common_words(theDict,10)
  print("\n\nWord Values ~~~~~")
  display_word_values(theDict)

if __name__ == "__main__":
    main()
