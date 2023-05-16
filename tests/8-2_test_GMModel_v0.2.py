import os

root = "/media/yslin/Avocet/"
# root = '/home/yslin/文件/'
wkdir = root + "[01]Projects/viscando/tests/"
# Set working directory to debug using interactive environment.
os.chdir(wkdir)
# os.chdir('D:/[01]Projects/viscando/tests/')

import sc_run
import sc_fitting
import numpy as np

# Correct the missing font problem.
from matplotlib import rcParams

rcParams.update(
    {
        "font.family": "sans-serif",
        "font.sans-serif": ["Liberation Sans"],
    }
)

# 1. It is unclear how the "initial" in the naturalitic setting matching to
# the model. I selected the first instance of non-zero value, meaning the
# Viscando sensor returned the first valid sample.
# 2. The "end_time" is set to 10 seconds in GM's model.

# Read a csv file. I used the manual selection, because there were only four.
# This could be a if-else. To save time and make it clear, I did not
# use a if-else or case syntax.

# This actually should be sharp deceleration. Short stop refers to a space-based definition.
# I will add a new behavior capturing procedure to catpure the space-based definition.
fn0 = wkdir + "extdata/4_short_stop.csv"
# fn1 = wkdir + 'extdata/5_priority_assertion.csv'
# fn2 = wkdir + 'extdata/6_hesitation.csv'
# fn3 = wkdir + 'extdata/7_early_yield.csv'
data = np.genfromtxt(fn0, delimiter=",", names=True)

# The name of the four columns: distance_ped, distance_veh, speed_ped, speed_veh.
# The number of rows:
ninteraction = data.shape[0]

plot_figure = False
N_AGENTS = 2
pyroot = "pydata/short_stop"
# pyroot =  'pydata/priority_assertion'
# pyroot =  'pydata/hesitation'
# pyroot =  'pydata/early_yield'
for i in range(0, data.shape[0]):
    distance_ped = data[i][0]
    distance_veh = data[i][1]
    speed_ped = data[i][2]
    speed_veh = data[i][3]

    # GM wants one of the two ways to simulate the results:
    # (1) Write a for loop of 100 times that iterative over the model simulation
    # change from "oVAoBEvoAI oDA oSNvoPF" (193 sets of the "optimial" parameter),
    # instead of "oVAoBEvoAI oEA oSNvoPF" (1091 sets of the "optimial" parameter)
    #
    # (2) Produce replicates for each of the "optimial" parameter set.
    # Each represent different individual interactions.
    #
    # The try (1) yields rather ambiguous results. He may do his procedure as
    # shown in his complex Python object oriented inheritance structure to
    # free up minimalizing metrics, criteria of selecting parameters,
    # moving parts of the equation and many more result-oriented selection.
    # This may result in a simulation result fitting the presented data. One
    # can also safely argue this is a RMSE similar procedure, minimizing the
    # difference between the model and the data.
    # (Note 1: GM's model was basdd on driver simulator data and his interpretion
    # of the results in a number of published papers employing video data.)
    # (Note 2: He had sent over two versions of the Jupyter notebook about which
    # is the correct way to use his model. In the limited of time, I cannot
    # pinpoint whether there is difference or where might not run as a correct,
    # as he is the expert of his model.)
    #
    # In light of the fact that I will not be able to increase the size of
    # the naturalistic data, such a result oriented method will not show meaningful
    # insight, except for the model owner. Neither will it refuse GM's model is
    # invalid because in some
    # part of his documented document may imply some nuances can be free up to
    # make his model fit the data and I have not free up such nuances.
    #
    # To make this task actionble instead of trying many of the seemingly endless
    # possibility, as well as proceeding, from a user's point-of-view, to run a
    # cross-validation test against a naturalistic data set, I have to do the
    # following:
    #
    stoch_model = sc_run.get_model_with_params("oVAoBEvoAIoDAoSNvoPF")

    for j in range(0, stoch_model.n_parameterisations):
        sim = sc_run.run_simulation(
            stoch_model,
            initial_cp_distances=(distance_ped, distance_veh),
            initial_speeds=(speed_ped, speed_veh),
            end_time=10,
            ped_prio=True,
            idx_parameterisation=j,
        )
        fn = pyroot + "/time_stamps" + str(i) + "-" + str(j) + ".txt"
        np.savetxt(fn, sim.time_stamps, delimiter=",")

        fn = pyroot + "/actual_end_time_" + str(i) + "-" + str(j) + ".txt"
        g = list()
        g.append(sim.actual_end_time)
        np.savetxt(fn, np.array(g), fmt="%.8f")

        i_plot_agents = range(N_AGENTS)
        agent_alpha = np.ones(N_AGENTS)
        for idx_agent, i_agent in enumerate(i_plot_agents):
            agent = sim.agents[i_agent]
            alpha = agent_alpha[idx_agent]
            acceleration = agent.trajectory.long_acc
            speed = agent.trajectory.long_speed
            distance2encounter = agent.signed_CP_dists
            other_perceived_speed = agent.other_agent.perception.states.x_perceived[
                1, :
            ]
            other_perceived_distance2encounter = (
                agent.other_agent.perception.states.x_perceived[0, :]
            )
            fn = (
                pyroot
                + "/acceleration_case"
                + str(i)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, acceleration, delimiter=",")
            fn = pyroot + "/speed" + str(i) + "-" + str(j) + "_" + str(i_agent) + ".txt"
            np.savetxt(fn, speed, delimiter=",")
            fn = (
                pyroot
                + "/other_perceived_speed"
                + str(i)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, other_perceived_speed, delimiter=",")
            fn = (
                pyroot
                + "/distance2encounter"
                + str(i)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, distance2encounter, delimiter=",")
            fn = (
                pyroot
                + "/other_distance2encounter"
                + str(i)
                + "-"
                + str(j)
                + "_"
                + str(i_agent)
                + ".txt"
            )
            np.savetxt(fn, other_perceived_distance2encounter, delimiter=",")
