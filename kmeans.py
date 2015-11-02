from mrjob.job import MRJob
import numpy as np

class KMeans(MRJob):
    
    def mapper(self, _, line):
        x = np.array(line.split()).astype('float')
        if x in clusters:
            yield "chars", len(line)
            yield "words", len(line.split())
            yield "lines", 1
        
    def reducer(self, key, values):
        yield key, sum(values)
        
if __name__ == "__main__":
    KMeans.run()