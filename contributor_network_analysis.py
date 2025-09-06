# Packages
import os
import pandas as pd
import networkx as nx

# Setting file path.
abspath = os.path.abspath(__file__)
dname = os.path.dirname(abspath)
os.chdir(dname)

# Loading data
main_repos_commit_count = pd.read_csv("/home/zachary/Documents/weighting_ethereum/main_repos_commits_count.csv")

# Rearranging the table and filtering out observations with zero commits.
main_repos_commit_long = main_repos_commit_count.melt(id_vars="user", var_name="repo", value_name="commits")
main_repos_commit_long = main_repos_commit_long[main_repos_commit_long["commits"] > 0]

# Initializing Graph
repo_graph = nx.Graph()

# Adding user and repo nodes.
repo_graph.add_nodes_from(main_repos_commit_long["user"], bipartite = "user")
repo_graph.add_nodes_from(main_repos_commit_long["repo"], bipartite = "repo")

# Adding edges.
edges = [(row["user"], row["repo"], {"weight": row["commits"]})
         for _, row in main_repos_commit_long.iterrows()]


repo_graph.add_edges_from(edges)

# Looking at connections between repositories. Idea is that repositories that share more users are more "important".

# Grabbing just the repo nodes.
repos = [n for n, d in repo_graph.nodes(data=True) if d["bipartite"] == "repo"]

only_repos = nx.bipartite.weighted_projected_graph(repo_graph, repos)

closeness = nx.closeness_centrality(only_repos)

closeness_df = pd.DataFrame.from_dict(closeness, orient="index", columns=["closeness"])
closeness_df = closeness_df.reset_index().rename(columns={"index": "repo"})

closeness_df.to_csv("closeness_centrality.csv")

# Second analysis only on the giant component of the graph.
components = list(nx.connected_components(only_repos))

largest_cc = max(components, key = len)

repo_graph_cc = only_repos.subgraph(largest_cc).copy()

closeness_cc = nx.closeness_centrality(repo_graph_cc)

# Checking
sorted_closeness_cc = sorted(closeness_cc.items(), key=lambda x: x[1], reverse=True)
print("Top 10 repos by closeness (largest connected component):")
for repo, score in sorted_closeness_cc[:10]:
    print(repo, score)

# Saving
closeness_cc_df = pd.DataFrame.from_dict(closeness_cc, orient="index", columns=["closeness"])
closeness_cc_df = closeness_cc_df.reset_index().rename(columns={"index": "repo"})

closeness_cc_df.to_csv("closeness_giant_centrality.csv")