#include <erl_nif.h>
#include <iostream>
#include <memory>
#include <map>
#include <opencv4/opencv2/core/core.hpp>
#include <opencv4/opencv2/highgui/highgui.hpp>
#include <opencv4/opencv2/imgproc/imgproc.hpp>

static std::map<uint64_t, cv::Mat> matMap;
static uint64_t matIdRes = 0;

typedef struct {
    uint64_t id;
} CvImage;

static void cvMatDtor(ErlNifEnv* env, void* mat)
{
    CvImage* m = (CvImage*)mat;
    matMap.erase(m->id);
}

static ErlNifResourceType* matRt = NULL;

#define MOTOR_SCALE 50

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
    ErlNifResourceType* rt = enif_open_resource_type(env, NULL,
            "OpenCv_Mat",
            cvMatDtor,
            ERL_NIF_RT_CREATE, NULL);
    if (rt == NULL) {
        return -1;
    }
    assert(matRt == NULL);
    matRt = rt;
    return 0;
}

static ERL_NIF_TERM do_load_png_file(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    char filename[1024];
    if (enif_get_string(env, argv[0], filename, 1024, ERL_NIF_LATIN1) <= 0)
        return enif_make_badarg(env);
    cv::Mat readImage = cv::imread(filename, cv::IMREAD_GRAYSCALE);
    uint64_t id = matIdRes++;
    matMap.insert({id, readImage});
    CvImage* imageRes = (CvImage*)enif_alloc_resource(matRt, sizeof(CvImage));
    imageRes->id = id;
    ERL_NIF_TERM ret = enif_make_resource(env, imageRes);
    return ret;
}

static ERL_NIF_TERM do_count_mats(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    uint64_t count = matMap.size();
    return enif_make_uint64(env, count);
}


static ERL_NIF_TERM do_crop_image(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CvImage* imageResource;
    int xMax, yMax, xLimit, yLimit;
    if(argc != 3) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], matRt, (void**)&imageResource))
        return enif_make_badarg(env);
    auto foundElement = matMap.find(imageResource->id);
    if (foundElement == matMap.end()) return enif_make_badarg(env);
    cv::Mat image = matMap.at(imageResource->id);
    cv::Size imageSize = image.size();

    if(!enif_get_int(env, argv[1], &xMax))
        return enif_make_badarg(env);
    if(!enif_get_int(env, argv[2], &yMax))
        return enif_make_badarg(env);

    if (imageSize.width < xMax)
    {
        xLimit = imageSize.width;
    }
    else
    {
        xLimit = xMax;
    }
    if (imageSize.height < yMax)
    {
        yLimit = imageSize.height;
    }
    else
    {
        yLimit = yMax;
    }
    cv::Mat croppedImage = image(cv::Rect(0,0,xLimit,yLimit));
    cv::Mat croppedImageCopy;
    croppedImage.copyTo(croppedImageCopy);
    uint64_t id = matIdRes++;
    matMap.insert({id, croppedImageCopy});
    CvImage* imageRes = (CvImage*)enif_alloc_resource(matRt, sizeof(CvImage));
    imageRes->id = id;
    ERL_NIF_TERM ret = enif_make_resource(env, imageRes);
    return ret;
}

static ERL_NIF_TERM do_get_dimensions(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CvImage* imageResource;
    if(argc != 1) return enif_make_badarg(env);

    if(!enif_get_resource(env, argv[0], matRt, (void**)&imageResource))
        return enif_make_badarg(env);
    auto foundElement = matMap.find(imageResource->id);
    if (foundElement == matMap.end()) return enif_make_badarg(env);
    cv::Mat image = matMap.at(imageResource->id);
    cv::Size imageSize = image.size();
    return enif_make_tuple2(env, enif_make_int(env, imageSize.width), enif_make_int(env, imageSize.height));
}

static ERL_NIF_TERM do_png_encode(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CvImage* imageResource;
    if (argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], matRt, (void**)&imageResource))
        return enif_make_badarg(env);
    auto foundElement = matMap.find(imageResource->id);
    if (foundElement == matMap.end()) return enif_make_badarg(env);
    cv::Mat image = matMap.at(imageResource->id);
    std::vector<uchar> buf;
    cv::imencode(".png", image, buf, std::vector<int>());
    ERL_NIF_TERM encodedTerm;
    uint8_t* encodedBuf = enif_make_new_binary(env, buf.size(), &encodedTerm);
    memcpy(encodedBuf, &buf[0], buf.size());
    return encodedTerm;
}

ERL_NIF_TERM make_laser_command(ErlNifEnv* env, ERL_NIF_TERM laser_command_class, int arg1, int arg2)
{
    return enif_make_tuple4(env,
            enif_make_atom(env, "laser_command"),
            laser_command_class,
            enif_make_int(env, arg1),
            enif_make_int(env, arg2));
}

ERL_NIF_TERM make_laser_op(ErlNifEnv* env, std::vector<ERL_NIF_TERM> commands, int startX, int startY)
{
    if (commands.size() == 0) return enif_make_badarg(env);
    return enif_make_tuple4(env,
            enif_make_atom(env, "laser_operation"),
            enif_make_int(env, startX),
            enif_make_int(env, startY),
            enif_make_list_from_array(env, (ERL_NIF_TERM*)&commands.at(0), commands.size()));
}

ERL_NIF_TERM make_simple_laser_op(ErlNifEnv* env, ERL_NIF_TERM command, int startX, int startY)
{
    return enif_make_tuple4(env,
            enif_make_atom(env, "laser_operation"),
            enif_make_int(env, startX),
            enif_make_int(env, startY),
            enif_make_list1(env, command));
}


static ERL_NIF_TERM do_image_to_plan(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
    CvImage* imageResource;
    if (argc != 1)
        return enif_make_badarg(env);
    if(!enif_get_resource(env, argv[0], matRt, (void**)&imageResource))
        return enif_make_badarg(env);

    auto foundElement = matMap.find(imageResource->id);
    if (foundElement == matMap.end()) return enif_make_badarg(env);
    cv::Mat image = matMap.at(imageResource->id);
    cv::Size imageSize = image.size();
    std::vector<ERL_NIF_TERM> imageOps;
    int xLimit = imageSize.width;
    int yLimit = imageSize.height;

    for (int r = 0; r < yLimit; r++)
    {
        int direction = 0;
        if ((r & 0x1) == 0) //even rows left to right
            direction = 1;
        else
            direction = -1; // odd rows right to left
        if (direction == 1) // left to right
        {
            for(int c = 0; c < xLimit; c++)
            {
                auto pixHere = image.at<uchar>(r, c);
                if(pixHere != 0)
                {
                    uint32_t extend = 0;
                    if (pixHere == image.at<uchar>(r, c+1))
                    {
                        // scan left to right and 
                        // figure out how long the continuous stripe is
                        for (int c2 = c+1;
                                (c2 < xLimit) && (image.at<uchar>(r,c2) == pixHere);
                                c2++,extend++);
                    }
                    ERL_NIF_TERM burnCmd =
                        make_laser_command(env,
                                enif_make_atom(env, "BH"),
                                (extend + 1)*MOTOR_SCALE,
                                pixHere);

                    ERL_NIF_TERM burnOp =
                        make_simple_laser_op(env, burnCmd, c*MOTOR_SCALE, r*MOTOR_SCALE);
                    imageOps.push_back(burnOp);
                    c += extend; // account for contiguous block
                }
            }
        }
        else // right to left
        {
            for(int c = xLimit; c >= 0; c--)
            {
                auto pixHere = image.at<uchar>(r, c);
                if(pixHere != 0)
                {
                    int32_t extend = 0;
                    if (pixHere == image.at<uchar>(r, c-1))
                    {
                        // scan left to right and 
                        // figure out how long the continuous stripe is
                        for (int c2 = c-1;
                                (c2 >= 0) && (image.at<uchar>(r,c2) == pixHere);
                                c2--,extend--);
                    }
                    ERL_NIF_TERM burnCmd =
                        make_laser_command(env,
                                enif_make_atom(env, "BH"),
                                (extend - 1)*MOTOR_SCALE,
                                pixHere);

                    ERL_NIF_TERM burnOp =
                        make_simple_laser_op(env, burnCmd, c*MOTOR_SCALE, r*MOTOR_SCALE);
                    imageOps.push_back(burnOp);
                    c += extend; // account for contiguous block
                }
            }
        }
    }
    if (imageOps.size() == 0) return enif_make_badarg(env);
    return enif_make_list_from_array(env, (ERL_NIF_TERM*)&imageOps.at(0), imageOps.size());
}

static ErlNifFunc nif_funcs[] = {
    {"do_load_png_file", 1, do_load_png_file},
    {"do_count_mats", 0, do_count_mats},
    {"do_crop_image", 3, do_crop_image},
    {"do_get_dimensions", 1, do_get_dimensions},
    {"do_png_encode", 1, do_png_encode},
    {"do_image_to_plan", 1, do_image_to_plan}
};

ERL_NIF_INIT(blue_scribe_plan_image, nif_funcs, load, NULL, NULL, NULL)
