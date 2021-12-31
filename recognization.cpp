
 /*
 ���ڶ�Ӧ�������ά����ʶ��
 ����pcl_recognitionģ�����ά����ʶ��
 ������˵�������������ʹ�ö�Ӧ�����㷨��
 �Ա㽫3D������ƥ��׶�֮���õ�һ��㵽���Ӧ�����ϵ���ǰ�����е�ģ��ʵ���С�
 ÿ����Ⱥ������һ�����ܵĳ����е�ģ��ʵ����
 ��Ӧ�ķ����㷨�����
 �任����ʶ��ǰ�ĳ����У�
 ģ�͵������ɶ�λ�˹��� 6DOF pose estimation ��
 ִ������
 ./correspondence_grouping ../milk.pcd ../milk_cartoon_all_small_clorox.pcd -c -k
 ��1�����㷨������  ���������� Э�������PCA ��ά����άƽ�� ���㷨������
    PCA��άԭ�� http://blog.codinglabs.org/articles/pca-tutorial.html
    ǰ��˵���Ƕ�ά����һάʱ�����������������һ��ɢ�ҵ���ά����,������������㷨�ߣ�
    1����ÿһ���㣬ȡ�ٽ��㣬����ȡ���ٽ���50���㣬��Ȼ���õ�K-D��
    2�����ٽ�����PCA��ά������������άƽ����,��������õ����ƽ��һ����������ƽ��(����ƽ���ϲſ��Ծ����ܷ�ɢ��
    3����ƽ��ķ��߾��Ǹõ�ķ����ˣ��������ķ�����������ȡ�ĸ�����Ҫ�����ٽ����͹������
 ��2���²����˲�ʹ�þ��Ȳ����������������ظ����²������õ��ؼ���
 ��3��Ϊkeypoints�ؼ������SHOT������
 ��4�����洢����KDTreeƥ���������ƣ�����������ƥ�䣩���Ʒ���õ�ƥ����� ���� ���ƥ���ϵ
 ��5���ο�֡�������/����һ���Ծ���õ� ƥ�����cluster  ƽ�ƾ���� ƥ���Թ�ϵ
 ��6��������ʾ ƽ�ƾ��� T ��ģ�͵��ư�T�任����ʾ �Լ���ʾ ���֮�������
 */

#include <pcl/io/pcd_io.h>// �ļ����豸��д
#include <pcl/point_cloud.h>//����pcl��������
#include <pcl/correspondence.h>//�����㷨 ��Ӧ��ʾ����ʵ��֮���ƥ�䣨���磬�㣬�������ȣ���
 // ����
#include <pcl/features/normal_3d_omp.h>//����������
#include <pcl/features/shot_omp.h> //������ shot������ 0��1
// https://blog.csdn.net/bengyanluo1542/article/details/76061928?locationNum=9&fps=1
// (Signature of Histograms of OrienTations)����ֱ��ͼ����
#include <pcl/features/board.h>
// �˲�
#include <pcl/filters/uniform_sampling.h>//���Ȳ��� �˲�
// ʶ��
#include <pcl/recognition/cg/hough_3d.h>//hough����
#include <pcl/recognition/cg/geometric_consistency.h> //����һ����
// ���ӻ�
#include <pcl/visualization/pcl_visualizer.h>//���ӻ�
// kdtree
#include <pcl/kdtree/kdtree_flann.h>// kdtree ���ٽ�������
#include <pcl/kdtree/impl/kdtree_flann.hpp>
// ת��
#include <pcl/common/transforms.h>//����ת�� ת������
// �����в���
#include <pcl/console/parse.h>//�����в�������
// ����
/*
shot ��������
���췽�����Բ�ѯ��pΪ���Ĺ���뾶Ϊr �����������ؾ��򡢷�λ������3�����򻮷�����
���о���2�Σ���λ8�Σ�Ϊ���ͼ�о���ֻ������4����������2�λ�������
���������򻮷ֳ�32���ռ�����
��ÿ���ռ���������������������ķ���nv�����ĵ�p����np֮��ļн�����cos��=nv��np��
�ٸ��ݼ��������ֵ������ÿһ���ռ�����ĵ�������ֱ��ͼͳ�ƣ�����11������
�Լ��������й�һ����ʹ�öԵ����ܶȾ���³���ԣ��õ�һ��352ά������32*11=352����
��ԭ���ģ�Unique Signatures of Histograms for Local Surface��
*/
typedef pcl::PointXYZRGBA PointType;//PointXYZRGBA���ݽṹ ������ λ�ú���ɫ
typedef pcl::Normal NormalType;//��������
typedef pcl::ReferenceFrame RFType;//�ο�֡
typedef pcl::SHOT352 DescriptorType;//SHOT���������ݽṹ��32*11=352��

std::string model_filename_;//ģ�͵��ļ���
std::string scene_filename_;//�����ļ���

//�㷨���� Algorithm params
bool show_keypoints_(false);
bool show_correspondences_(false);
bool use_cloud_resolution_(false);
bool use_hough_(true);
float model_ss_(0.01f);//ģ�Ͳ�����0.01
float scene_ss_(0.03f);//����������0.03
float rf_rad_(0.015f);    //0.015
float descr_rad_(0.03f);   //0.03
float cg_size_(0.01f);//���� ����ռ�����ÿ��bin�Ĵ�С
float cg_thresh_(1.0f);//������ֵ5

//�������ݵ�ַsource/milk.pcd source/milk_cartoon_all_small_clorox.pcd -c
//�Լ�����������source/model_all_filter_bigcorner.pcd source/model_all_filter.pcd -c

// ��ӡ������Ϣ �����÷�
void
showHelp(char* filename)
{
    std::cout << std::endl;
    std::cout << "***************************************************************************" << std::endl;
    std::cout << "*                                                                         *" << std::endl;
    std::cout << "*             Correspondence Grouping Tutorial - Usage Guide              *" << std::endl;
    std::cout << "*                                                                         *" << std::endl;
    std::cout << "***************************************************************************" << std::endl << std::endl;
    std::cout << "Usage: " << filename << " model_filename.pcd scene_filename.pcd [Options]" << std::endl << std::endl;
    std::cout << "Options:" << std::endl;
    std::cout << "     -h:                     Show this help." << std::endl;
    std::cout << "     -k:                     Show used keypoints." << std::endl;//�ؼ���
    std::cout << "     -c:                     Show used correspondences." << std::endl;//�����㷨
    std::cout << "     -r:                     Compute the model cloud resolution and multiply" << std::endl;
    std::cout << "                             each radius given by that value." << std::endl;
    std::cout << "     --algorithm (Hough|GC): Clustering algorithm used (default Hough)." << std::endl;//�����㷨
    std::cout << "     --model_ss val:         Model uniform sampling radius (default 0.01)" << std::endl;//ģ�Ͳ�����
    std::cout << "     --scene_ss val:         Scene uniform sampling radius (default 0.03)" << std::endl;//����������
    std::cout << "     --rf_rad val:           Reference frame radius (default 0.015)" << std::endl;//�ο�֡ �뾶
    std::cout << "     --descr_rad val:        Descriptor radius (default 0.02)" << std::endl;//�����Ӽ���뾶
    std::cout << "     --cg_size val:          Cluster size (default 0.01)" << std::endl;//����
    std::cout << "     --cg_thresh val:        Clustering threshold (default 5)" << std::endl << std::endl;//����������ֵ
}

// �����в�������
void
parseCommandLine(int argc, char* argv[])
{
    // -h ��ӡ������ϢShow help
    if (pcl::console::find_switch(argc, argv, "-h"))
    {
        showHelp(argv[0]);
        exit(0);
    }

    //  ģ�ͺͳ����ļ� Model & scene filenames
    std::vector<int> filenames;

    
    filenames = pcl::console::parse_file_extension_argument(argc, argv, ".pcd");
    if (filenames.size() != 2)
    {
        std::cout << "Filenames missing.\n";
        showHelp(argv[0]);
        exit(-1);
    }

    model_filename_ = argv[filenames[0]];//ģ���ļ�
    scene_filename_ = argv[filenames[1]];//�����ļ�
    
    //model_filename_ = "source/milk.pcd";//ģ���ļ�
    //scene_filename_ = "source/milk_cartoon_all_small_clorox.pcd";//�����ļ�

    //���� ��Ϊ���� Program behavior
    //show_correspondences_ = true;//��ʾ��Ӧ����
    //�������ݵ�ַsource/milk.pcd source/milk_cartoon_all_small_clorox.pcd -c
    //�������ݵ�ַsource/model_all_filter_bigcorner.pcd source/model_all_filter.pcd -c



    if (pcl::console::find_switch(argc, argv, "-k"))
    {
        show_keypoints_ = true;//��ʾ�ؼ���
    }
    if (pcl::console::find_switch(argc, argv, "-c"))
    {
        show_correspondences_ = true;//��ʾ��Ӧ����
    }
    if (pcl::console::find_switch(argc, argv, "-r"))
    {
        use_cloud_resolution_ = true;//���Ʒֱ���
    }
    
    // �����㷨 
    std::string used_algorithm;
    if (pcl::console::parse_argument(argc, argv, "--algorithm", used_algorithm) != -1)
    {
        if (used_algorithm.compare("Hough") == 0)
        {
            use_hough_ = true;
        }
        else if (used_algorithm.compare("GC") == 0)
        {
            use_hough_ = false;
        }
        else
        {
            std::cout << "Wrong algorithm name.\n";
            showHelp(argv[0]);
            exit(-1);
        }
    }

    //һ���������General parameters
    pcl::console::parse_argument(argc, argv, "--model_ss", model_ss_);
    pcl::console::parse_argument(argc, argv, "--scene_ss", scene_ss_);
    pcl::console::parse_argument(argc, argv, "--rf_rad", rf_rad_);
    pcl::console::parse_argument(argc, argv, "--descr_rad", descr_rad_);
    pcl::console::parse_argument(argc, argv, "--cg_size", cg_size_);
    pcl::console::parse_argument(argc, argv, "--cg_thresh", cg_thresh_);
}

// ������Ʒֱ��� ���� ÿ������������֮��ľ���� ��ƽ��ֵ
double
computeCloudResolution(const pcl::PointCloud<PointType>::ConstPtr& cloud)
{
    double res = 0.0;
    int n_points = 0;
    int nres;//�ٽ��������
    std::vector<int> indices(2);// ����
    std::vector<float> sqr_distances(2);//����ƽ�� 
    pcl::search::KdTree<PointType> tree;//�������� kdtree
    tree.setInputCloud(cloud);//�������

    for (size_t i = 0; i < cloud->size(); ++i)//����ÿһ����
    {
        
        if (!isfinite((*cloud)[i].x))//�޳� NAN��
        {
            continue;
        }
        //Considering the second neighbor since the first is the point itself.
        nres = tree.nearestKSearch(i, 2, indices, sqr_distances);//�õ����Ǿ���ƽ��
        if (nres == 2)//������һ��Ϊ����ڶ���Ϊ�����Լ����Լ������һ����
        {
            res += sqrt(sqr_distances[1]);//�����ź����
            ++n_points;
        }
    }
    if (n_points != 0)
    {
        res /= n_points;//�����֮��ľ��� ��ƽ��ֵ
    }
    return res;
}

// ������
int
main(int argc, char* argv[])
{
    //======== ��1�������в�������========================
    parseCommandLine(argc, argv);

    //======== ��2���½���Ҫ�� ָ�����===================
    pcl::PointCloud<PointType>::Ptr model(new pcl::PointCloud<PointType>());//ģ�͵���
    pcl::PointCloud<PointType>::Ptr model_keypoints(new pcl::PointCloud<PointType>());//ģ�͵��ƵĹؼ��� ����
    pcl::PointCloud<PointType>::Ptr scene(new pcl::PointCloud<PointType>());//��������
    pcl::PointCloud<PointType>::Ptr scene_keypoints(new pcl::PointCloud<PointType>());//�������Ƶ� �ؼ��� ����
    pcl::PointCloud<NormalType>::Ptr model_normals(new pcl::PointCloud<NormalType>());//ģ�͵��Ƶ� ��������
    pcl::PointCloud<NormalType>::Ptr scene_normals(new pcl::PointCloud<NormalType>());//�������Ƶ� ��������
    pcl::PointCloud<DescriptorType>::Ptr model_descriptors(new pcl::PointCloud<DescriptorType>());//ģ�͵��� ������� ����������
    pcl::PointCloud<DescriptorType>::Ptr scene_descriptors(new pcl::PointCloud<DescriptorType>());//�������� ������� ����������

    //
    //=======��3���������==========================
    //
    if (pcl::io::loadPCDFile(model_filename_, *model) < 0)//ģ�͵���
    {
        std::cout << "Error loading model cloud." << std::endl;
        showHelp(argv[0]);
        return (-1);
    }
    if (pcl::io::loadPCDFile(scene_filename_, *scene) < 0)//��������
    {
        std::cout << "Error loading scene cloud." << std::endl;
        showHelp(argv[0]);
        return (-1);
    }

    //
    //======��4�����÷ֱ��� ����==========================
    //
    if (use_cloud_resolution_)//ʹ�÷ֱ���
    {
        float resolution = static_cast<float> (computeCloudResolution(model));//����ֱ���
        if (resolution != 0.0f)
        {
            model_ss_ *= resolution;//���²���
            scene_ss_ *= resolution;
            rf_rad_ *= resolution;
            descr_rad_ *= resolution;
            cg_size_ *= resolution;
        }

        std::cout << "Model resolution:       " << resolution << std::endl;
        std::cout << "Model sampling size:    " << model_ss_ << std::endl;
        std::cout << "Scene sampling size:    " << scene_ss_ << std::endl;
        std::cout << "LRF support radius:     " << rf_rad_ << std::endl;
        std::cout << "SHOT descriptor radius: " << descr_rad_ << std::endl;
        std::cout << "Clustering bin size:    " << cg_size_ << std::endl << std::endl;
    }

    //
    //========��5�����㷨������============== 
    //
    pcl::NormalEstimationOMP<PointType, NormalType> norm_est;//��� ���㷨��ģ�� OpenMP
  //  pcl::NormalEstimation<PointType, NormalType> norm_est;//��� ���㷨��ģ�� OpenMP
    norm_est.setKSearch(10);//���10���� Э�������PCA�ֽ� ���� ��������
    pcl::search::KdTree<pcl::PointXYZ>::Ptr tree(new pcl::search::KdTree<pcl::PointXYZ>());
    //norm_est.setSearchMethod (tree);// ���ģʽ ����Ҫ���� �����㷨
    norm_est.setInputCloud(model);//ģ�͵���
    norm_est.compute(*model_normals);//ģ�͵��Ƶķ�������

    norm_est.setInputCloud(scene);//��������
    norm_est.compute(*scene_normals);//�������Ƶķ�������

    //
    //=======��6���²����˲�ʹ�þ��Ȳ����������������ظ����²������õ��ؼ���=========
    //

    pcl::UniformSampling<PointType> uniform_sampling;//�²����˲�ģ��
    uniform_sampling.setInputCloud(model);//ģ�͵���
    uniform_sampling.setRadiusSearch(model_ss_);//ģ�͵��������뾶
    uniform_sampling.filter(*model_keypoints);//�²����õ��Ĺؼ���
    std::cout << "Model total points: " << model->size() << "; Selected Keypoints: " << model_keypoints->size() << std::endl;

    uniform_sampling.setInputCloud(scene);//��������
    uniform_sampling.setRadiusSearch(scene_ss_);//���������뾶
    uniform_sampling.filter(*scene_keypoints);//�²����õ��Ĺؼ���
    std::cout << "Scene total points: " << scene->size() << "; Selected Keypoints: " << scene_keypoints->size() << std::endl;


    //
    //========��7��Ϊkeypoints�ؼ������SHOT������Descriptor===========
    //
    pcl::SHOTEstimationOMP<PointType, NormalType, DescriptorType> descr_est;//shot������
    descr_est.setRadiusSearch(descr_rad_);

    descr_est.setInputCloud(model_keypoints);
    descr_est.setInputNormals(model_normals);
    descr_est.setNumberOfThreads(4);
    descr_est.setSearchSurface(model);
    descr_est.compute(*model_descriptors);//ģ�͵���������

    descr_est.setInputCloud(scene_keypoints);
    descr_est.setInputNormals(scene_normals);
    descr_est.setSearchSurface(scene);
    descr_est.compute(*scene_descriptors);//��������������

    //
    //========��8�����洢����KDTreeƥ���������ƣ�����������ƥ�䣩���Ʒ���õ�ƥ�����====== 
    //
    pcl::CorrespondencesPtr model_scene_corrs(new pcl::Correspondences());//���ƥ������

    pcl::KdTreeFLANN<DescriptorType> match_search;//ƥ������
    match_search.setInputCloud(model_descriptors);//ģ�͵���������
    // �� ���������� Ϊ ģ�͵��Ƶ�ÿһ���ؼ��� ƥ��һ�� �����������Ƶ� ��
    for (size_t i = 0; i < scene_descriptors->size(); ++i)//������������
    {
        std::vector<int> neigh_indices(1);//����
        std::vector<float> neigh_sqr_dists(1);//�����Ӿ���
        
        if (!isfinite(scene_descriptors->at(i).descriptor[0])) //����NAN��
        {
            continue;
        }
        
        int found_neighs = match_search.nearestKSearch(scene_descriptors->at(i), 1, neigh_indices, neigh_sqr_dists);
        if (found_neighs == 1 && neigh_sqr_dists[0] < 0.25f) //��ģ�͵����� �� ���� �������Ƶ�i shot�����Ӿ��� <0.25 �ĵ�  
        {
            pcl::Correspondence corr(neigh_indices[0], static_cast<int> (i), neigh_sqr_dists[0]);
            //   neigh_indices[0] Ϊģ�͵����� �� �������� ��   scene_descriptors->at (i) ��ѵ�ƥ�� ����Ϊ neigh_sqr_dists[0]  
            model_scene_corrs->push_back(corr);
        }
    }
    std::cout << "Correspondences found: " << model_scene_corrs->size() << std::endl;//ƥ����ƶ� ����

    //
    //===========��9��ִ�о���================
    //
    std::vector<Eigen::Matrix4f, Eigen::aligned_allocator<Eigen::Matrix4f> > rototranslations;//�任���� ��ת������ƽ�ƾ���
  // ��eigen�еĹ̶���С����ʹ��STL������ʱ�����ֱ��ʹ�þͻ���� ��Ҫʹ�� Eigen::aligned_allocator ���뼼��
    std::vector<pcl::Correspondences> clustered_corrs;//ƥ��� �໥���ߵ�����
  // clustered_corrs[i][j].index_query ģ�͵� ����
  // clustered_corrs[i][j].index_match ������ ����

    //
        //=========����ο�֡��Hough��Ҳ���ǹؼ��㣩=========
        //
    pcl::PointCloud<RFType>::Ptr model_rf(new pcl::PointCloud<RFType>());//ģ�Ͳο�֡
    pcl::PointCloud<RFType>::Ptr scene_rf(new pcl::PointCloud<RFType>());//�����ο�֡
    //======����ģ�Ͳο�֡���������Ƶķ��������ƣ����ߣ��ο�֡��
    pcl::BOARDLocalReferenceFrameEstimation<PointType, NormalType, RFType> rf_est;
    rf_est.setFindHoles(true);
    rf_est.setRadiusSearch(rf_rad_); //���������뾶

    rf_est.setInputCloud(model_keypoints);//ģ�͹ؼ���
    rf_est.setInputNormals(model_normals);//��������
    rf_est.setSearchSurface(model);//ģ�͵���
    rf_est.compute(*model_rf);//����ģ�Ͳο�֡

    rf_est.setInputCloud(scene_keypoints);//�����ؼ���
    rf_est.setInputNormals(scene_normals);//��������
    rf_est.setSearchSurface(scene);//��������
    rf_est.compute(*scene_rf);//�����ο�֡

    //  ���� ����ķ��� Clustering
   //���������ľ��࣬�����ֲ�ͬ��ʵ���ĳ����е�ģ��
    pcl::Hough3DGrouping<PointType, PointType, RFType, RFType> clusterer;
    clusterer.setHoughBinSize(cg_size_);//����ռ�����ÿ��bin�Ĵ�С
    clusterer.setHoughThreshold(cg_thresh_);//��ֵ
    clusterer.setUseInterpolation(true);
    clusterer.setUseDistanceWeight(false);

    clusterer.setInputCloud(model_keypoints);//ģ�͵��� �ؼ���
    clusterer.setInputRf(model_rf);//ģ�͵��Ʋο�֡
    clusterer.setSceneCloud(scene_keypoints);//�������ƹؼ���
    clusterer.setSceneRf(scene_rf);//�������Ʋο�֡
    clusterer.setModelSceneCorrespondences(model_scene_corrs);//�������ϵ

    //clusterer.cluster (clustered_corrs);//���ϳ�����Ķ���
    clusterer.recognize(rototranslations, clustered_corrs);

    /*
    //  ʹ�� Hough3D 3D���� �㷨Ѱ��ƥ���
    if (use_hough_)
    {
        //
        //=========����ο�֡��Hough��Ҳ���ǹؼ��㣩=========
        //
        pcl::PointCloud<RFType>::Ptr model_rf(new pcl::PointCloud<RFType>());//ģ�Ͳο�֡
        pcl::PointCloud<RFType>::Ptr scene_rf(new pcl::PointCloud<RFType>());//�����ο�֡
        printf_s("����ο�֡�ɹ�\n");
        //======����ģ�Ͳο�֡���������Ƶķ��������ƣ����ߣ��ο�֡��
        pcl::BOARDLocalReferenceFrameEstimation<PointType, NormalType, RFType> rf_est;
        rf_est.setFindHoles(true);
        rf_est.setRadiusSearch(rf_rad_); //���������뾶

        rf_est.setInputCloud(model_keypoints);//ģ�͹ؼ���
        rf_est.setInputNormals(model_normals);//��������
        rf_est.setSearchSurface(model);//ģ�͵���
        rf_est.compute(*model_rf);//����ģ�Ͳο�֡

        rf_est.setInputCloud(scene_keypoints);//�����ؼ���
        rf_est.setInputNormals(scene_normals);//��������
        rf_est.setSearchSurface(scene);//��������
        rf_est.compute(*scene_rf);//�����ο�֡
        printf_s("����ģ�Ͳο�֡�ɹ�\n");

        //  ���� ����ķ��� Clustering
       //���������ľ��࣬�����ֲ�ͬ��ʵ���ĳ����е�ģ��
        pcl::Hough3DGrouping<PointType, PointType, RFType, RFType> clusterer;
        clusterer.setHoughBinSize(cg_size_);//����ռ�����ÿ��bin�Ĵ�С
        clusterer.setHoughThreshold(cg_thresh_);//��ֵ
        clusterer.setUseInterpolation(true);
        clusterer.setUseDistanceWeight(false);

        clusterer.setInputCloud(model_keypoints);//ģ�͵��� �ؼ���
        clusterer.setInputRf(model_rf);//ģ�͵��Ʋο�֡
        clusterer.setSceneCloud(scene_keypoints);//�������ƹؼ���
        clusterer.setSceneRf(scene_rf);//�������Ʋο�֡
        clusterer.setModelSceneCorrespondences(model_scene_corrs);//�������ϵ

        //clusterer.cluster (clustered_corrs);//���ϳ�����Ķ���
        //std::cout << "����û������" << endl;
        clusterer.recognize(rototranslations, clustered_corrs);
    }
    else // ����ʹ�ü���һ�������� Using GeometricConsistency
    {
        pcl::GeometricConsistencyGrouping<PointType, PointType> gc_clusterer;
        gc_clusterer.setGCSize(cg_size_);//���ü���һ���ԵĴ�С
        gc_clusterer.setGCThreshold(cg_thresh_);//��ֵ

        gc_clusterer.setInputCloud(model_keypoints);
        gc_clusterer.setSceneCloud(scene_keypoints);
        gc_clusterer.setModelSceneCorrespondences(model_scene_corrs);

        //gc_clusterer.cluster (clustered_corrs);//���ϳ�����Ķ���
        gc_clusterer.recognize(rototranslations, clustered_corrs);
    }
    */
    //
    //========��10�����ʶ����=====Output results=====
    // 
    // �ҳ�����ģ���Ƿ��ڳ����г���
    std::cout << "Model instances found: " << rototranslations.size() << std::endl;
    for (size_t i = 0; i < rototranslations.size(); ++i)
    {
        std::cout << "\n    Instance " << i + 1 << ":" << std::endl;
        std::cout << "        Correspondences belonging to this instance: " << clustered_corrs[i].size() << std::endl;

        // ��ӡ ���������ģ�͵���ת������ƽ�ƾ���  rotation matrix and translation vector
        // [R t]
        Eigen::Matrix3f rotation = rototranslations[i].block<3, 3>(0, 0);//��ת����
        Eigen::Vector3f translation = rototranslations[i].block<3, 1>(0, 3);//ƽ������

        printf("\n");
        printf("            | %6.3f %6.3f %6.3f | \n", rotation(0, 0), rotation(0, 1), rotation(0, 2));
        printf("        R = | %6.3f %6.3f %6.3f | \n", rotation(1, 0), rotation(1, 1), rotation(1, 2));
        printf("            | %6.3f %6.3f %6.3f | \n", rotation(2, 0), rotation(2, 1), rotation(2, 2));
        printf("\n");
        printf("        t = < %0.3f, %0.3f, %0.3f >\n", translation(0), translation(1), translation(2));
    }

    //
    //======���ӻ� Visualization===============================
    //
    pcl::visualization::PCLVisualizer viewer("Correspondence Grouping");//��Ӧ��
    viewer.addPointCloud(scene, "scene_cloud");//��ӳ�������
    viewer.setBackgroundColor(255,255,255);

    pcl::PointCloud<PointType>::Ptr off_scene_model(new pcl::PointCloud<PointType>());// ģ�͵��� �任��ĵ���
    pcl::PointCloud<PointType>::Ptr off_scene_model_keypoints(new pcl::PointCloud<PointType>());//�ؼ���

    if (show_correspondences_ || show_keypoints_) //���ӻ� ƽ�ƺ��ģ�͵���
    {
        //  We are translating the model so that it doesn't end in the middle of the scene representation
         //����Ҫ�������ģ�ͽ�����ת��ƽ�ƣ�ʹ���ڿ��ӻ�������м�λ�� x�Ḻ����ƽ��1����λ 
        pcl::transformPointCloud(*model, *off_scene_model, Eigen::Vector3f(-2, 0, 0), Eigen::Quaternionf(1, 0, 0, 0));
        pcl::transformPointCloud(*model_keypoints, *off_scene_model_keypoints, Eigen::Vector3f(-2, 0, 0), Eigen::Quaternionf(1, 0, 0, 0));

        pcl::visualization::PointCloudColorHandlerCustom<PointType> off_scene_model_color_handler(off_scene_model, 44, 34, 133);
        viewer.addPointCloud(off_scene_model, off_scene_model_color_handler, "off_scene_model");//��ʾƽ�ƺ��ģ�͵���
    }

    if (show_keypoints_)//���ӻ� �����ؼ��� �� ģ�͹ؼ���
    {
        pcl::visualization::PointCloudColorHandlerCustom<PointType> scene_keypoints_color_handler(scene_keypoints, 0, 0, 255);
        viewer.addPointCloud(scene_keypoints, scene_keypoints_color_handler, "scene_keypoints");//���ӻ������ؼ���
        viewer.setPointCloudRenderingProperties(pcl::visualization::PCL_VISUALIZER_POINT_SIZE, 5, "scene_keypoints");
        // ���ӻ������ ��С

        pcl::visualization::PointCloudColorHandlerCustom<PointType> off_scene_model_keypoints_color_handler(off_scene_model_keypoints, 0, 0, 255);//���ӻ�ģ�͹ؼ���
        viewer.addPointCloud(off_scene_model_keypoints, off_scene_model_keypoints_color_handler, "off_scene_model_keypoints");
        viewer.setPointCloudRenderingProperties(pcl::visualization::PCL_VISUALIZER_POINT_SIZE, 5, "off_scene_model_keypoints");
    }

    for (size_t i = 0; i < rototranslations.size(); ++i)//���� ģ���ڳ����� ƥ��� ����
    {
        pcl::PointCloud<PointType>::Ptr rotated_model(new pcl::PointCloud<PointType>());//��ƥ��任���� ģ�͵���
        pcl::transformPointCloud(*model, *rotated_model, rototranslations[i]);//��ģ�͵��ư�ƥ��ı任������ת

        std::stringstream ss_cloud;//�ַ��������
        ss_cloud << "instance" << i;//ʶ�����ʵ��

        pcl::visualization::PointCloudColorHandlerCustom<PointType> rotated_model_color_handler(rotated_model, 255, 0, 0);
        viewer.addPointCloud(rotated_model, rotated_model_color_handler, ss_cloud.str());//���ģ�Ͱ�ʶ�� �任����任�� ��ʾ

        if (show_correspondences_)//��ʾƥ��� ����
        {
            for (size_t j = 0; j < clustered_corrs[i].size(); ++j)
            {
                std::stringstream ss_line;//ƥ��� ���� �ַ���
                ss_line << "correspondence_line" << i << "_" << j;
                PointType& model_point = off_scene_model_keypoints->at(clustered_corrs[i][j].index_query);//ģ�͵�
                PointType& scene_point = scene_keypoints->at(clustered_corrs[i][j].index_match);//������

                //  ��ʾ����ƥ�����ÿһ��ƥ����֮�������
                viewer.addLine<PointType, PointType>(model_point, scene_point, 0, 255, 0, ss_line.str());
            }
        }
    }

    while (!viewer.wasStopped())
    {
        viewer.spinOnce();
    }

    return (0);
}