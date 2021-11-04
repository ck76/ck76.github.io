[TOC]

UPDATE: 吴恩达最近在Deeplearning.ai的[线上直播](https://link.zhihu.com/?target=https%3A//www.bilibili.com/video/BV1WB4y1P7Hw)中也聊了聊MLOps，他认为我们应该将重点转向以数据为中心来开发机器学习系统，而MLOps是为项目持续提供高质量的数据的重要保障。在本文中也提到了不少数据相关的实践与工具介绍，包括数据版本，数据质量测试，血缘管理，线上数据监控，feature store等。包括在上一篇文章中，我们也介绍了在模型开发过程中，通过各种数据分析手段来提升模型的一些尝试，有兴趣的同学可以移步观看：

- https://zhuanlan.zhihu.com/p/330577488

我们在整体实践过程中，除了数据方面的挑战，另外感触比较深的是如何将模型持续部署更新，并稳定在线上运行方面的挑战。这也是MLOps相对于以往的DevOps部署软件包不同的新挑战。因此各类与机器学习相关的测试手段，与云计算结合的灵活training/serving部署，以及各类监控机制是本文关注的另一大重点。

## 什么是MLOps

顾名思义，MLOps就是机器学习时代的DevOps。它的主要作用就是连接模型构建团队和业务，运维团队，建立起一个标准化的模型开发，部署与运维流程，使得企业组织能更好的利用机器学习的能力来促进业务增长。

举个简单的例子，几年前我们对于机器学习的印象主要是拿到一堆excel/csv数据，通过notebook等尝试做一些模型实验，最终产出一个预测结果。但对于这个预测结果如何使用，对业务产生了什么影响，大家可能都不是很有概念。这就很容易导致机器学习项目一直停留在实验室阶段，一个接一个做POC，但都没法成功“落地”。

最近几年，大家对于机器学习项目落地愈发重视起来，对业务的理解，模型应用流程等都做的越来越好，也有越来越多的模型被部署到真实的业务场景中。但是当业务真实开始使用的时候，就会对模型有各种各样的需求反馈，算法工程师们就开始需要不断迭代开发，频繁部署上线。随着业务的发展，模型应用的场景也越来越多，管理和维护这么多模型系统就成了一个切实的挑战。

回顾这个发展，是不是感觉似曾相识？20年前软件行业在数字化演进道路上也遇到过类似的挑战。我们从部署一个Web服务到要部署几十甚至上百个不同的应用，在各种规模化交付方面的挑战之下，诞生了DevOps技术。像虚拟化，云计算，持续集成/发布，自动化测试等软件工程领域的各类最佳实践基本都跟这个方向有关。在不远的将来，或许智能模型也会与今天的软件系统一样普遍。一个企业需要使用非常多的业务系统来实现数字化流程，同样也需要非常多的模型来实现数据驱动的智能决策，衍生出更多与模型相关的开发运维，权限，隐私，安全性，审计等企业级需求。

因此最近几年，MLOps也逐渐成为了一个热门话题。有了好的MLOps实践，算法工程师一方面能更专注于擅长的模型构建过程，减少对模型部署运维等方面的“感知”，另一方面也让模型开发迭代的方向更加清晰明确，切实为业务产生价值。就像今日的软件工程师很少需要关注运行环境，测试集成，发布流程等细节，但却做到了一天数次发布的敏捷高效，未来算法工程师应该也能更专注于数据insights获取方面，让模型发布成为几乎无感又快速的自动化流程。

另外对于MLOps的专业分析，可以参考[MLOpsRoadmap](https://link.zhihu.com/?target=https%3A//github.com/cdfoundation/sig-mlops/blob/master/roadmap/2020/MLOpsRoadmap2020.md)。他们对MLOps的定义是：“the extension of the DevOps methodology to include Machine Learning and Data Science assets as first class citizens within the DevOps ecology”，后面我们也会从各类ML assets的角度，例如特征，模型等角度出发来分析一些MLOps中的一些需求与挑战。

## MLOps的各个步骤

从大的方面看，MLOps分3个步骤：

1. 项目设计，包括需求收集，场景设计，数据可用性检查等。
2. 模型开发，包括数据工程，模型工程，以及评估验证等。
3. 模型运维，包括模型部署，CI/CD/CT工作流，监控与调度触发等。

DevOps通过缩短开发部署的时间来更快地迭代软件产品，使得公司业务不断进化。MLOps的逻辑也是通过相似的自动化和迭代形式，加快企业从数据到insights的价值获取速度。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t9peb8j30k00f0myk.jpg)



MLOps的核心要解决的问题之一是缩短模型开发部署的迭代周期，即各类efficiency问题。从Algorithmia的2020年的[这份报告](https://link.zhihu.com/?target=https%3A//info.algorithmia.com/hubfs/2019/Whitepapers/The-State-of-Enterprise-ML-2020/Algorithmia_2020_State_of_Enterprise_ML.pdf)中可以看到，很大一部分公司需要31-90天上线一个模型，其中有18%的公司需要90天以上来上线一个模型。且在中小型公司中，算法工程师花在模型部署方面的时间比例也明显偏多。MLOps希望通过更标准化自动化的流程与基础设施支持，来提升模型交付的整体效率。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31ta4o0aj30k00gldg7.jpg)



另外一方面，MLOps还希望能提供一个企业内各个角色无缝协作的平台，让业务，数据，算法，运维等角色能更高效率的进行协作，提升业务价值产出，即transparency的需求。这对于我们思考MLOps与各个角色的交互形式，产品设计等都是一个重要的出发点。后面我们的详细讨论中也会反复印证这两个核心诉求。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t89ku7j30k00e1q4e.jpg)



## MLOps的原则

### Automation

在整个workflow中所有可以自动化的环节，我们都应该进行自动化，从数据的接入到最后的部署上线。Google那篇经典的MLOps指导中就提出了3个层级的自动化，非常值得借鉴，后面我们会详细介绍。

### Continuous

一说起DevOps，大家就很容易联想到CI/CD，也从侧面印证这条原则的重要性。MLOps在持续集成，持续部署，持续监控的基础上，还增加了持续训练的概念，即模型在线上运行过程中可以持续得到自动化的训练与更新。我们在设计开发机器学习系统时，要持续思考各个组件对“持续”性的支持，包括流程中用到的各种artifacts，他们的版本管理和编排串联等。

### Versioning

版本化管理也是DevOps的重要最佳实践之一，在MLOps领域，除了pipeline代码的版本管理，数据，模型的版本管理属于新涌现的需求点，也对底层infra提出了新的挑战。

### Experiment Tracking

实验管理可以理解为version control中commit message的增强。对于涉及模型构建相关的代码改动，我们都应该能记录当时对应的数据，代码版本，以及对应的模型artifacts存档，作为后续分析模型，选择具体上线的版本的重要依据。

### Testing

机器学习系统中主要涉及到3种不同的pipeline，分别是数据pipeline，模型pipeline和应用pipeline（类似于模型与应用系统的集成）。针对这3个pipeline，需要构建对应的数据特征测试，模型测试以及应用infra测试，确保整体系统的输出与预期的业务目标相符，达到将数据insights转化为业务价值的目的。这方面Google的ML test score是一个很好的参考。

### Monitoring

监控也是一项软件工程的传统最佳实践。上面提到的ML test score中也有一部分是与监控相关。除了传统的系统监控，例如日志，系统资源等方面外，机器学习系统还需要对输入输出数据，模型相关指标进行监控，确保预测的质量和效率，并在出现异常情况时自动触发一些应对机制，例如数据或模型的降级，模型的重新训练与部署等。

### Reproducibility

与传统软件系统的确定性行为不同，机器学习中带有不少“随机化”的成分，这对各种问题的排查，版本回滚，输出效果的确定性都提出了一定的挑战。因此我们在开发过程中也需要时刻将可复现原则放在心上，设计相应的最佳实践（如设定随机数种子，运行环境等各类依赖的版本化等）。

## MLOps流程细节

我们来看下具体的机器学习项目流程，并对每一个模块中MLOps需要提供的支持进行详细的展开。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t6eg1bj30k00abwf8.jpg)



### 项目设计

项目设计所需要受到的重视程度毋庸置疑，之前在Fullstack Deep Learning的课程介绍中我们也有很大的篇幅来进行介绍。在MLOps领域，我们应该为这部分的工作也设计一系列的标准与文档。业界可以参考的材料也有很多，例如[Machine Learning Canvas](https://link.zhihu.com/?target=https%3A//www.louisdorard.com/machine-learning-canvas)，[Data Landscape](https://link.zhihu.com/?target=https%3A//www.canvasgeneration.com/canvas/data-landscape/)等。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t4js95j30k00fzgmm.jpg)



### 数据接入

数据接入方面，我们会利用成熟的数据平台，例如各类数据仓库，数据湖或实时数据源等。对于接入到平台后的数据存储，可以优先考虑带有数据版本支持的组件，例如Delta Lake等。当然也可以采用DVC或自行元数据维护等方案来进行ML相关数据资产的管理。

### 数据分析

在数据接入后，一般会需要进行各类EDA分析。传统的做法一般是使用notebook来进行交互式分析，但对于分析结果的保存管理，共享协作，数据更新后的自动刷新，高级交互分析能力方面，原生notebook本身还是有不少缺陷，难以很好满足。有一些研究与产品在这个方向上做了一些改进，例如Polynote，Facets，Wrattler等。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t38eloj31a40u0dml.jpg)



### 数据检查

对于接入的原始数据，通常会出现各类质量问题或数据类型，含义，分布等方面的变化。而机器学习pipeline即使在数据有变化的情况下基本也能顺利运行成功，造成意向不到的各种“静默失败”问题，其排查处理会相当艰难，耗费算法工程师大量的时间精力。因此设置各类自动化的数据检查就显得尤为重要，例如Tensorflow Data Validation就是这方面比较知名的一个library。

O'Reilly在20年做了个关于数据质量方面的调研，发现企业中存在的主要数据问题如下所示：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t274nyj30t40gb0u0.jpg)



除上述问题外涉及到模型应用，各类drift的探测也相当重要，比如输入数据的分布变化(data drift)，或者输入数据与预测目标之间关系的变化(concept drift)。为了应对这些数据质量问题，我们需要根据不同的业务领域设计相应的数据质量检查模板，并结合具体情况进行各类属性，统计，甚至基于模型的数据问题检查。

一些常见的检查包括：

- Schema检查
- null/nan/inf等数值异常检查
- 格式检查
- 范围检查，如数值范围，类别范围等
- 唯一性检查
- 映射检查
- 统计检查
- 领域相关检查

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31t0bglmj30k007fmxi.jpg)



### 数据工程

这部分的工作包括数据清洗，数据转换，特征工程。根据业务形态的不同，这部分所占的比重可能会各不相同，但总体上来说这部分在整个模型开发过程中占的比重和遇到的挑战是比较大的。包括：

- 对于大量数据处理逻辑的管理，调度执行和运维处理。
- 对于数据版本的管理和使用。
- 对于数据复杂依赖关系的管理，例如数据血缘。
- 对于不同形式数据源的兼容和逻辑一致性，例如lambda架构对batch，realtime两种数据源类型的处理。
- 对于离线和在线数据服务需求的满足，例如离线模型预测和在线模型服务。

以数据血缘为例，一个经常遇到的场景是当我们发现下游数据有问题时，可以通过数据血缘图快速定位上游依赖项，分别进行排查。而在问题修复后，又可以通过血缘关系重新运行所有影响的下游节点，执行相关测试验证。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31szdgqrj30k00cuq3s.jpg)



在建模应用领域，有不少数据处理特征工程方面的操作和应用会更加复杂，例如：

- 需要使用模型来生成特征，例如各种表达学习中学到的embedding信息。
- 需要考虑特征计算生成的实践开销与其所带来的模型效果提升的权衡。
- 跨组织的特征共享与使用。

在这些挑战下，feature store的概念逐渐兴起。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31sygtjsj31sf0u0wj3.jpg)



这方面有一个非常不错的汇总网站可以参考：[Feature Store for ML](https://link.zhihu.com/?target=https%3A//www.featurestore.org/) ，上面列举了各种开源/闭源feature store实现的信息，包括耳熟能详的Michelangelo，FBLearner，也有像开源界的Feast，Hopsworks等。

Feature Store要解决的核心问题上面也基本提到了，主要包括：

- 一个统一的特征管理中心，包括特征的各类元数据，计算逻辑，特征版本，数据血缘等，方便在组织内部管理和共享特征的使用。
- 特征的生成支持，包括各类数据源的接入，特征自动化计算，持久化，backfill等。很多时候业务都包含了batch, streaming两大类型的数据源，这部分感觉可以从lambda架构中学习一些经验。
- 特征的消费支持，也可以分为offline和online两大类。Offline就是训练阶段的特征提取支持，而online则是model serving时需要获取特征的支持。这里需要解决的一个比较重要的问题就是training-serving skew。
- 特征的各类监控支持，包括特征生成的开销，特征的统计信息，数据质量，模型应用的精度，特征重要度等等。

从上面这些核心的功能点，可以看到Feature Store的一些比较特别的技术架构设计：

- 在存储方面，大多数的解决方案都会区分提供offline和online服务的存储系统，因为两者的访问pattern分别是大批量获取和小批量低延迟获取。例如Michelangelo里offline存储使用了Hive，而online的存储利用了Cassandra的KV查询能力。Hopsworks的offline存储用了Hudi，可以比较好的支持数据版本。Feast的offline和online存储分别是BigQuery和BigTable/Redis，不过这个选型对不同云平台的支持会有一些挑战。像一些新近的“湖仓一体”技术，或许未来也有机会成为Feature Store的底层技术支持。
- 在计算引擎方面，算法建模的同学最习惯的肯定还是Python生态，而batch和streaming两块比较流行的框架是Spark和Flink。如何保持算法建模的灵活性，同时又要兼顾线上运行的稳定和一致性已经是不小的挑战了，再加上batch和streaming计算逻辑的统一，难度就更大了。Feast在这方面利用了Apache Beam，可以同时支持Spark和Flink后端，但是构建feature的灵活度肯定就没有原生Python那么高了。也有不少公司例如Uber，Pinterest，Twitter都开发了相应的DSL，也是一种思路。国内应该有不少公司也是DSL/SQL的思路？印象中美团有篇文章就提到了他们开发了特征工程相关的DSL。
- 数据版本方面，看起来用Delta Lake/Hudi/Iceberg等的解决方案还比较少，很多都是自行开发的支持。个人猜测跟复杂的serving pattern支持有一定关系，比如Delta Lake对于实时查询访问的支持就会比较困难。
- 数据格式的支持，相比于标准的结构化数据，算法训练过程中不同库需要消费的数据格式也不尽相同，还需要考虑到online serving的格式支持。像offline情况下可以选择parquet存储，支持更高效的数据压缩和传输，而online情况下可能需要json/protobuf之类便于API消费的数据格式。

未来Feature Store也还有很多想象发展空间，例如与Cloud-native技术结合形成的新技术架构；在计算逻辑上的抽象统一，并提高可组合能力；Feature Store中存储的各类元信息，也有机会被meta learning等技术手段进行更加深入的应用。

### 模型构建

模型构建方面总体来说是受到关注与讨论比较多的部分，有非常多成熟的机器学习框架来帮助用户训练模型，评估模型效果。这块MLOps需要提供的支持包括：

- 模型开发过程中的结果评估与分析，包括指标误差分析，模型解释工具，可视化等。
- 模型本身的各类元数据管理，实验信息，结果记录(指标，详细数据，图表)，文档(model card)等。
- 模型训练的版本化管理，包括各种依赖库，训练代码，数据，以及最终生成的模型等。
- 模型在线更新和离线再训练，增量训练的支持。
- 一些模型策略的集成，例如embedding的提取与保存，stratified/ensemble模型支持，transfer learning之类的增量训练支持等。
- AutoML类的自动模型搜索，模型选择的支持。

在模型实验管理方面，可以借鉴的产品有MLflow，neptune.ai，Weights and Biases等。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31swku3dj30k00cl75i.jpg)



从以模型为中心的角度来看，与feature store一样，我们需要进一步引入model repository，支持链接到实验结果的记录，以及模型部署状态，线上监控反馈等信息的打通。各类与模型运维相关的操作都可以在这个组件中进行支持。开源方面的实现可以关注[ModelDB](https://link.zhihu.com/?target=https%3A//github.com/VertaAI/modeldb/)，以及TF serving中类似saved_model_cli的设计考虑。

### 集成测试

完成数据和模型两大块pipeline的构建后，我们需要执行一系列的测试验证来评估是否能将新模型部署到线上。包括：

- 模型预测方面的测试，如精度，预测稳定性，特定case回归等。
- Pipeline执行效率的测试，如整体执行时间，计算资源开销量等。
- 与业务逻辑集成的测试，如模型输出的格式是否符合下游消费者的要求等。

参考Google经典的[ML Test Score](https://link.zhihu.com/?target=https%3A//research.google/pubs/pub46555/)，具体有以下各类测试：

- 数据验证测试，除了对原始数据输入方面的数据质量检查外，在机器学习的pipeline中做的各类数据特征处理，也需要用一系列的测试来验证其符合预期。
- 特征重要度测试，对于各类构建的特征，我们需要确保其在模型中的贡献度，以免造成计算资源和特征存储上的浪费。对于无用的特征也需要及时清理，控制pipeline的整体复杂度。
- 隐私审计等相关要求测试。
- 模型训练测试，模型应该能够利用数据进行有效训练，如loss会在训练中呈下降趋势。并且预测目标相对于业务目标是有提升作用。
- 模型时效性测试，与旧版本模型的效果进行比对，测试模型指标的下降速度，并设计模型的重训练周期。
- 模型开销测试，确保复杂模型的训练时间投入产出比，相比简单的规则和基线模型有显著的效果提升。
- 模型指标测试，确保模型的测试集验证或特定回归问题验证能够通过。这里对于测试集的选择尤为重要，需要考虑业务的特性进行精心设计。
- 模型公平性测试，对敏感信息，例如性别，年龄等，模型应该在不同特征分组的情况下表现出公平的预测概率。
- 模型扰动测试，对模型的输入数据进行微小的扰动，其输出值的变动范围应该符合预期。
- 模型版本比对测试，对于没有进行重大更新的模型，例如例行触发的retrain，两个模型版本的输出之间不应该有过大的差别。
- 模型训练框架测试，例如重复执行2次相同的训练，应该给出稳定可复现的结果。
- 模型API测试，对于模型服务的输入输出做验证测试。
- 集成测试，对整个pipeline进行运行和验证，确保各个环节的衔接正确。
- 线上测试，在模型部署但对外服务前，需要进行与离线环境相同的一系列验证测试，确保运行结果无误。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31supmipj317d0fitaa.jpg)



这方面TFX也同样提供了不少组件支持，例如TFMA，ModelBlessing等。

### 模型部署

通过测试后，我们就可以把模型部署上线啦。这里又根据业务形态的不同分成很多不同的类型，需要考虑不同的发布方式，例如：

- Batch预测pipeline
- 实时模型服务
- Edge device部署，如手机app，浏览器等

模型部署的assets除了模型本身外，也需要包含end-to-end测试用例, 测试数据和相应的结果评估等。可以在模型部署完成后再执行一遍相关测试用例，确保开发和部署环境中得到的结果一致。

对于输出较为critical的模型，还需要考虑一系列model governance的需求满足。例如在模型部署前需要进行各类人工审核，并设计相应的sign-off机制。顺带一提responsible AI近年来也是越来越受到重视，在MLOps中的各个环节也需要关注相应功能的支持。

除了模型外，特征处理部分的部署也是一大挑战，尤其是在实时应用场景下。目前常见的处理思路有两类，对于比较标准化的特征处理操作，如归一化，log转换等，可以把处理操作作为模型整体计算图的一部分来一起打包部署，比如使用TF transform；而对于比较复杂的特征例如用户embedding等，可以通过feature store的方案来进行部署。

### 模型服务

在模型服务流程中，也需要有许多检查与策略的融入，才能保证整体输出的可靠性和合理性。各类测试检查的逻辑可以借鉴前面的测试环节的例子。另外还有一些更复杂的需求例如让一部分的用户bypass模型，确保能收集到一些不受模型输出影响的数据，还原真实用户的数据分布。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31stbadyj30k00b3jrt.jpg)

模型服务在形式上也非常多变：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31srw4igj30k00f0wfi.jpg)

因此涉及到的话题也非常多，例如实时模型服务需要考虑模型的序列化，异构硬件利用，推理性能优化(quantization，pruning，distillation)，动态batching，部署的形式(container, serverless)，serving缓存，model streaming等。要是涉及到在线更新，还需要考虑online learning的实现。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31sqh1f8j30k00bhwf6.jpg)



对于edge deploy，我们需要考虑模型的不同打包方式，模型转换与优化(TFLite)等。甚至还可以做hybrid形式的serving或联邦学习，例如像智能音箱，可以在设备端部署一个简单的模型来接收唤醒指令，而将后续复杂的问答发送到云端的复杂模型进行处理。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31spnuj1j30k00f0dgu.jpg)



在上述模型部署步骤完成时一般也不算是正式发布，一般会使用一些策略来逐渐用新模型来替代旧模型，包括shadow model，canary部署，A/B测试，甚至MAB自动模型选择策略等。这方面使用Istio等云原生sidecar方案能够获得很好的灵活性。

在云原生时代Kubeflow中提供的一系列serverless serving，弹性伸缩，流量管理，以及附加组件(异常检测，模型解释)等方面的能力非常强大，值得学习：

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31snnrnqj30k00f975o.jpg)



关于云原生与AI系统的结合和各种新的机遇与挑战，或许下次有机会我们再单独写一篇文章来讨论。

### 模型监控

最后，对于线上模型的运行，我们需要持续进行监控，包括：

- 模型依赖组件的监控，例如数据版本，上游系统等
- 模型输入数据的监控，确保schema与分布的一致性
- 离线特征构建与线上特征构建输出的一致性监控，例如可以对一些样本进行抽样，比对线上线下结果，或者监控分布统计值
- 模型数值稳定性的监控，对NaN和Inf等情况进行记录
- 模型计算资源开销方面的监控
- 模型metric方面的监控
- 模型更新周期的监控，确保没有长时间未更新的模型
- 下游消费数量的监控，确保没有处于“废弃”状态的模型
- 对于排查问题有用的日志记录，尤其是涉及到客户端部署
- 对于提升模型有用的信息记录
- 外界攻击预防监控

上述的各类监控都要配合相应的自动/人工应对机制。

以模型效果监控为例，当效果出现下降时，我们需要及时介入排查处理，或触发重训练。对于重训练来说，需要综合考虑模型效果变化，数据更新频率，训练开销，部署开销，重新训练的提升度等，选择合适的时间点进行触发。虽然有很多模型也支持在线实时更新，但其稳定性控制，自动化测试等都缺少标准做法的参考，大多数情况下，重新训练往往比在线更新训练的效果和稳定性更好。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31smbv6ej30k00ejq40.jpg)

而如果出现了依赖数据的问题，我们也可以设计一系列的降级策略，例如使用最近一版正常的历史数据，或者丢弃一些非核心特征，使用更基础的模型/策略给出预测等。

另外这里还有一个比较有意思的trade-off，如果环境变化较快，而模型重训练的代价又很高，有时候可以考虑使用更简答一些的模型策略，往往对于环境变化的敏感度没有那么高，但代价是可能会有一些效果上的损失。

## 流程串联

Google的[这篇文章](https://link.zhihu.com/?target=https%3A//cloud.google.com/solutions/machine-learning/mlops-continuous-delivery-and-automation-pipelines-in-machine-learning)中，提出了3个level的MLOps流程自动化，将上述我们介绍的各个流程中可以自动化的部分进行了整体的串联，堪称MLOps的最佳实践之一。其中两个关键的自动化提升是pipeline自动化和CI/CD/CT自动化。另外一个比较核心的思想是模型部署并不只是部署一个模型对外提供服务的API，而是把整个pipeline进行打包部署。另外一个值得参考的方法论来自于Martin Fowler的[CD4ML](https://link.zhihu.com/?target=https%3A//martinfowler.com/articles/cd4ml.html)，其中还包含了很多具体组件的选择建议。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31slskinj30k00f00ul.jpg)



在整体的串联过程中，一些通用的依赖项有：

- 版本控制系统，包括数据，代码，和各类机器学习相关artifacts。
- 测试与构建系统，可以将各类运行逻辑在版本更新后自动执行相应测试，通过后打包成pipeline执行的组件镜像。
- 部署系统，可以将pipeline整体部署到应用环境，包括线上服务和客户端等。
- 模型中心，储存已经训练好的模型，对于训练时间较长的场景来说尤为重要。
- Feature store，存储各类特征，并服务于离线场景的批量消费和在线场景的实时查询消费。
- ML meta store，存储实验训练中产生的各类数据，包括实验名称，使用的数据，代码版本信息，超参数，模型预测相关的数据和图表等。
- Pipeline框架，串联一系列工作流程的执行框架，包括调度执行，断点续跑，失败自动重试，自动并行等等特性。

这些依赖组件中有不少是MLOps中出现的新需求，业界也开始有各类对应产品的涌现，例如Michelangelo，FBLearner，BigHead，MLflow，Kubeflow，TFX，Feast等等。但目前看起来各个组件还远没有达到像Web开发持续集成那样的标准化和成熟程度。例如对于workflow/pipeline组件的选择，可以参考[这个调研](https://link.zhihu.com/?target=https%3A//ploomber.io/posts/survey/)。CI/CD方面，传统的Jenkins，GoCD，CircleCI，Spinnaker等基本也可以满足需求，当然也可以考虑DVC出品的CML，更加针对机器学习场景来定制。Arize AI的这篇[整体ML infra的介绍](https://link.zhihu.com/?target=https%3A//ai-infrastructure.org/maximizing-ml-infrastructure-tools-for-production-workloads-arize-ai/)包含的scope更加全面，对于MLOps中各个组件的选型都可以提供一些参考。对应的开源方面的资源可以参考[awesome production ML](https://link.zhihu.com/?target=https%3A//github.com/EthicalML/awesome-production-machine-learning)。

目前很多云平台例如GCP，Azure，Amazon等也都提供了一系列支持MLOps功能的服务，包括pipeline运行，模型管理，模型服务等，对于中小型公司来说也不失为很好的一个选择方向。

[https://landscape.lfai.foundation/](https://link.zhihu.com/?target=https%3A//landscape.lfai.foundation/) (二维码自动识别)



最后在设计选型过程中，可以根据以下这个canvas来进行思考规划。

![img](https://tva1.sinaimg.cn/large/008i3skNly1gw31sjk71pj30k00ea76b.jpg)



针对整个流程的开发演进，建议通过敏捷迭代的形式进行。即先开发一个基础的能跑通的pipeline，使用最基础的数据和简单模型，把整个流程搭建起来。后续通过业务反馈，再去发现整个流程中的重要改进点，逐渐去迭代交付。

## Summary

MLOps如果能做的好，可以获得很多回报。个人感觉其中价值最大的有两点，一是通过各种工程上的最佳实践，提升了团队整体开发交付模型的效率。二是由于项目运维成本的降低，我们将有机会大大提升机器学习类应用的scale能力，例如在企业内上线上千个模型来为各方面的业务场景产出价值。





- https://ml-ops.org/
- 从小作坊到智能中枢: MLOps简介 - 字节的文章 - 知乎 https://zhuanlan.zhihu.com/p/357897337