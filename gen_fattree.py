'''this file generates a simple fat tree in dot notation with few assumptions:
1) the root is always 1 node
2) all switches have same incoming and outgoing edges, except the root node
3) all switches have same fan out '''


import networkx as nx
import sys

def main(argv):
	if len(argv) < 4:
		print "usage: python get_fattree.py num_fan_out num_layers out_file(.dot)"
	else:
		num_fan_out = int(argv[1])
		num_layers = int(argv[2])
		out_file = argv[3]
		switches = ['s'+ str(i) for i in range(1, ((1 - (num_fan_out ** num_layers)) / (1 - num_fan_out))+1)]
		hosts = ['h' + str(i) for i in range(1, num_fan_out ** num_layers + 1)]
		nodes = switches + hosts
		g = nx.MultiDiGraph()
		g.add_nodes_from(nodes)
		for i in range(num_layers,0,-1):
			'''length of temp1 = (length of temp2) * num_fan_out'''
			temp1 = nodes[len(nodes) - (num_fan_out ** i):]
			nodes = nodes[:len(nodes) - (num_fan_out ** i)]
			temp2 = nodes[len(nodes) - (num_fan_out ** (i-1)):]
			for j in range(len(temp2)):
				for k in range(num_fan_out):
					p = num_fan_out ** (num_layers - i)
					if p == 1:
						g.add_edge(temp1[num_fan_out * j + k],temp2[j],attr_dict={'sport':1,'dport':k+1,'capacity':'1Gbps','cost':'1'})
						g.add_edge(temp2[j],temp1[num_fan_out * j + k],attr_dict={'sport':k+1,'dport':1,'capacity':'1Gbps','cost':'1'})

					else:
						for l in range(p):
							g.add_edge(temp1[num_fan_out * j + k],temp2[j],attr_dict={'sport':p+l+1,'dport': num_fan_out * k + l+1,'capacity':'1Gbps','cost':'1'})
							g.add_edge(temp2[j],temp1[num_fan_out * j + k],attr_dict={'sport':num_fan_out * k + l+1,'dport': p+l+1,'capacity':'1Gbps','cost':'1'})

		nx.write_dot(g,'./' + out_file + '.dot')



if __name__ == "__main__":
	main(sys.argv)
