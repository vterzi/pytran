! Math utilities


module stdlib_math
    use stdlib_kinds

    implicit none

    private

#include "../inc/proc.inc"

#define _OP dec_digits
    public :: _OP
    interface _OP
#define _ID _INTEGER
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#define _OP dec_exponent
    public :: _OP
    interface _OP
#define _ID _REAL
#include "../inc/decls.inc"
    end interface _OP
#undef _OP

#ifdef _HP
    real(HP), parameter, public :: &
        REAL_HP_INF = _REAL_HP_INF, &
        REAL_HP_NAN = _REAL_HP_NAN
    real(HP), parameter :: REAL_HP_DEC_POWERS(-9:5) = [ &
        0, tiny(0._hp) * epsilon(0._hp), &
        1e-7_hp, 1e-6_hp, 1e-5_hp, 1e-4_hp, 1e-3_hp, 1e-2_hp, 1e-1_hp, &
        1e0_hp, 1e1_hp, 1e2_hp, 1e3_hp, 1e4_hp, &
        REAL_HP_INF &
    ]
#endif
#ifdef _SP
    real(SP), parameter, public :: &
        REAL_SP_INF = _REAL_SP_INF, &
        REAL_SP_NAN = _REAL_SP_NAN
    real(SP), parameter :: REAL_SP_DEC_POWERS(-46:39) = [ &
        0._sp, tiny(0._sp) * epsilon(0._sp), &
        1e-44_sp, 1e-43_sp, 1e-42_sp, 1e-41_sp, 1e-40_sp, 1e-39_sp, 1e-38_sp, &
        1e-37_sp, 1e-36_sp, 1e-35_sp, 1e-34_sp, 1e-33_sp, 1e-32_sp, 1e-31_sp, &
        1e-30_sp, 1e-29_sp, 1e-28_sp, 1e-27_sp, 1e-26_sp, 1e-25_sp, 1e-24_sp, &
        1e-23_sp, 1e-22_sp, 1e-21_sp, 1e-20_sp, 1e-19_sp, 1e-18_sp, 1e-17_sp, &
        1e-16_sp, 1e-15_sp, 1e-14_sp, 1e-13_sp, 1e-12_sp, 1e-11_sp, 1e-10_sp, &
        1e-9_sp, 1e-8_sp, 1e-7_sp, 1e-6_sp, 1e-5_sp, 1e-4_sp, 1e-3_sp, &
        1e-2_sp, 1e-1_sp, 1e0_sp, 1e1_sp, 1e2_sp, 1e3_sp, 1e4_sp, 1e5_sp, &
        1e6_sp, 1e7_sp, 1e8_sp, 1e9_sp, 1e10_sp, 1e11_sp, 1e12_sp, 1e13_sp, &
        1e14_sp, 1e15_sp, 1e16_sp, 1e17_sp, 1e18_sp, 1e19_sp, 1e20_sp, &
        1e21_sp, 1e22_sp, 1e23_sp, 1e24_sp, 1e25_sp, 1e26_sp, 1e27_sp, &
        1e28_sp, 1e29_sp, 1e30_sp, 1e31_sp, 1e32_sp, 1e33_sp, 1e34_sp, &
        1e35_sp, 1e36_sp, 1e37_sp, 1e38_sp, &
        REAL_SP_INF &
    ]
#endif
#ifdef _DP
    real(DP), parameter, public :: &
        REAL_DP_INF = _REAL_DP_INF, &
        REAL_DP_NAN = _REAL_DP_NAN
    real(DP), parameter :: REAL_DP_DEC_POWERS(-325:309) = [ &
        0._dp, tiny(0._dp) * epsilon(0._dp), &
        1e-323_dp, 1e-322_dp, 1e-321_dp, 1e-320_dp, 1e-319_dp, 1e-318_dp, &
        1e-317_dp, 1e-316_dp, 1e-315_dp, 1e-314_dp, 1e-313_dp, 1e-312_dp, &
        1e-311_dp, 1e-310_dp, 1e-309_dp, 1e-308_dp, 1e-307_dp, 1e-306_dp, &
        1e-305_dp, 1e-304_dp, 1e-303_dp, 1e-302_dp, 1e-301_dp, 1e-300_dp, &
        1e-299_dp, 1e-298_dp, 1e-297_dp, 1e-296_dp, 1e-295_dp, 1e-294_dp, &
        1e-293_dp, 1e-292_dp, 1e-291_dp, 1e-290_dp, 1e-289_dp, 1e-288_dp, &
        1e-287_dp, 1e-286_dp, 1e-285_dp, 1e-284_dp, 1e-283_dp, 1e-282_dp, &
        1e-281_dp, 1e-280_dp, 1e-279_dp, 1e-278_dp, 1e-277_dp, 1e-276_dp, &
        1e-275_dp, 1e-274_dp, 1e-273_dp, 1e-272_dp, 1e-271_dp, 1e-270_dp, &
        1e-269_dp, 1e-268_dp, 1e-267_dp, 1e-266_dp, 1e-265_dp, 1e-264_dp, &
        1e-263_dp, 1e-262_dp, 1e-261_dp, 1e-260_dp, 1e-259_dp, 1e-258_dp, &
        1e-257_dp, 1e-256_dp, 1e-255_dp, 1e-254_dp, 1e-253_dp, 1e-252_dp, &
        1e-251_dp, 1e-250_dp, 1e-249_dp, 1e-248_dp, 1e-247_dp, 1e-246_dp, &
        1e-245_dp, 1e-244_dp, 1e-243_dp, 1e-242_dp, 1e-241_dp, 1e-240_dp, &
        1e-239_dp, 1e-238_dp, 1e-237_dp, 1e-236_dp, 1e-235_dp, 1e-234_dp, &
        1e-233_dp, 1e-232_dp, 1e-231_dp, 1e-230_dp, 1e-229_dp, 1e-228_dp, &
        1e-227_dp, 1e-226_dp, 1e-225_dp, 1e-224_dp, 1e-223_dp, 1e-222_dp, &
        1e-221_dp, 1e-220_dp, 1e-219_dp, 1e-218_dp, 1e-217_dp, 1e-216_dp, &
        1e-215_dp, 1e-214_dp, 1e-213_dp, 1e-212_dp, 1e-211_dp, 1e-210_dp, &
        1e-209_dp, 1e-208_dp, 1e-207_dp, 1e-206_dp, 1e-205_dp, 1e-204_dp, &
        1e-203_dp, 1e-202_dp, 1e-201_dp, 1e-200_dp, 1e-199_dp, 1e-198_dp, &
        1e-197_dp, 1e-196_dp, 1e-195_dp, 1e-194_dp, 1e-193_dp, 1e-192_dp, &
        1e-191_dp, 1e-190_dp, 1e-189_dp, 1e-188_dp, 1e-187_dp, 1e-186_dp, &
        1e-185_dp, 1e-184_dp, 1e-183_dp, 1e-182_dp, 1e-181_dp, 1e-180_dp, &
        1e-179_dp, 1e-178_dp, 1e-177_dp, 1e-176_dp, 1e-175_dp, 1e-174_dp, &
        1e-173_dp, 1e-172_dp, 1e-171_dp, 1e-170_dp, 1e-169_dp, 1e-168_dp, &
        1e-167_dp, 1e-166_dp, 1e-165_dp, 1e-164_dp, 1e-163_dp, 1e-162_dp, &
        1e-161_dp, 1e-160_dp, 1e-159_dp, 1e-158_dp, 1e-157_dp, 1e-156_dp, &
        1e-155_dp, 1e-154_dp, 1e-153_dp, 1e-152_dp, 1e-151_dp, 1e-150_dp, &
        1e-149_dp, 1e-148_dp, 1e-147_dp, 1e-146_dp, 1e-145_dp, 1e-144_dp, &
        1e-143_dp, 1e-142_dp, 1e-141_dp, 1e-140_dp, 1e-139_dp, 1e-138_dp, &
        1e-137_dp, 1e-136_dp, 1e-135_dp, 1e-134_dp, 1e-133_dp, 1e-132_dp, &
        1e-131_dp, 1e-130_dp, 1e-129_dp, 1e-128_dp, 1e-127_dp, 1e-126_dp, &
        1e-125_dp, 1e-124_dp, 1e-123_dp, 1e-122_dp, 1e-121_dp, 1e-120_dp, &
        1e-119_dp, 1e-118_dp, 1e-117_dp, 1e-116_dp, 1e-115_dp, 1e-114_dp, &
        1e-113_dp, 1e-112_dp, 1e-111_dp, 1e-110_dp, 1e-109_dp, 1e-108_dp, &
        1e-107_dp, 1e-106_dp, 1e-105_dp, 1e-104_dp, 1e-103_dp, 1e-102_dp, &
        1e-101_dp, 1e-100_dp, 1e-99_dp, 1e-98_dp, 1e-97_dp, 1e-96_dp, &
        1e-95_dp, 1e-94_dp, 1e-93_dp, 1e-92_dp, 1e-91_dp, 1e-90_dp, 1e-89_dp, &
        1e-88_dp, 1e-87_dp, 1e-86_dp, 1e-85_dp, 1e-84_dp, 1e-83_dp, 1e-82_dp, &
        1e-81_dp, 1e-80_dp, 1e-79_dp, 1e-78_dp, 1e-77_dp, 1e-76_dp, 1e-75_dp, &
        1e-74_dp, 1e-73_dp, 1e-72_dp, 1e-71_dp, 1e-70_dp, 1e-69_dp, 1e-68_dp, &
        1e-67_dp, 1e-66_dp, 1e-65_dp, 1e-64_dp, 1e-63_dp, 1e-62_dp, 1e-61_dp, &
        1e-60_dp, 1e-59_dp, 1e-58_dp, 1e-57_dp, 1e-56_dp, 1e-55_dp, 1e-54_dp, &
        1e-53_dp, 1e-52_dp, 1e-51_dp, 1e-50_dp, 1e-49_dp, 1e-48_dp, 1e-47_dp, &
        1e-46_dp, 1e-45_dp, 1e-44_dp, 1e-43_dp, 1e-42_dp, 1e-41_dp, 1e-40_dp, &
        1e-39_dp, 1e-38_dp, 1e-37_dp, 1e-36_dp, 1e-35_dp, 1e-34_dp, 1e-33_dp, &
        1e-32_dp, 1e-31_dp, 1e-30_dp, 1e-29_dp, 1e-28_dp, 1e-27_dp, 1e-26_dp, &
        1e-25_dp, 1e-24_dp, 1e-23_dp, 1e-22_dp, 1e-21_dp, 1e-20_dp, 1e-19_dp, &
        1e-18_dp, 1e-17_dp, 1e-16_dp, 1e-15_dp, 1e-14_dp, 1e-13_dp, 1e-12_dp, &
        1e-11_dp, 1e-10_dp, 1e-9_dp, 1e-8_dp, 1e-7_dp, 1e-6_dp, 1e-5_dp, &
        1e-4_dp, 1e-3_dp, 1e-2_dp, 1e-1_dp, 1e0_dp, 1e1_dp, 1e2_dp, 1e3_dp, &
        1e4_dp, 1e5_dp, 1e6_dp, 1e7_dp, 1e8_dp, 1e9_dp, 1e10_dp, 1e11_dp, &
        1e12_dp, 1e13_dp, 1e14_dp, 1e15_dp, 1e16_dp, 1e17_dp, 1e18_dp, &
        1e19_dp, 1e20_dp, 1e21_dp, 1e22_dp, 1e23_dp, 1e24_dp, 1e25_dp, &
        1e26_dp, 1e27_dp, 1e28_dp, 1e29_dp, 1e30_dp, 1e31_dp, 1e32_dp, &
        1e33_dp, 1e34_dp, 1e35_dp, 1e36_dp, 1e37_dp, 1e38_dp, 1e39_dp, &
        1e40_dp, 1e41_dp, 1e42_dp, 1e43_dp, 1e44_dp, 1e45_dp, 1e46_dp, &
        1e47_dp, 1e48_dp, 1e49_dp, 1e50_dp, 1e51_dp, 1e52_dp, 1e53_dp, &
        1e54_dp, 1e55_dp, 1e56_dp, 1e57_dp, 1e58_dp, 1e59_dp, 1e60_dp, &
        1e61_dp, 1e62_dp, 1e63_dp, 1e64_dp, 1e65_dp, 1e66_dp, 1e67_dp, &
        1e68_dp, 1e69_dp, 1e70_dp, 1e71_dp, 1e72_dp, 1e73_dp, 1e74_dp, &
        1e75_dp, 1e76_dp, 1e77_dp, 1e78_dp, 1e79_dp, 1e80_dp, 1e81_dp, &
        1e82_dp, 1e83_dp, 1e84_dp, 1e85_dp, 1e86_dp, 1e87_dp, 1e88_dp, &
        1e89_dp, 1e90_dp, 1e91_dp, 1e92_dp, 1e93_dp, 1e94_dp, 1e95_dp, &
        1e96_dp, 1e97_dp, 1e98_dp, 1e99_dp, 1e100_dp, 1e101_dp, 1e102_dp, &
        1e103_dp, 1e104_dp, 1e105_dp, 1e106_dp, 1e107_dp, 1e108_dp, 1e109_dp, &
        1e110_dp, 1e111_dp, 1e112_dp, 1e113_dp, 1e114_dp, 1e115_dp, 1e116_dp, &
        1e117_dp, 1e118_dp, 1e119_dp, 1e120_dp, 1e121_dp, 1e122_dp, 1e123_dp, &
        1e124_dp, 1e125_dp, 1e126_dp, 1e127_dp, 1e128_dp, 1e129_dp, 1e130_dp, &
        1e131_dp, 1e132_dp, 1e133_dp, 1e134_dp, 1e135_dp, 1e136_dp, 1e137_dp, &
        1e138_dp, 1e139_dp, 1e140_dp, 1e141_dp, 1e142_dp, 1e143_dp, 1e144_dp, &
        1e145_dp, 1e146_dp, 1e147_dp, 1e148_dp, 1e149_dp, 1e150_dp, 1e151_dp, &
        1e152_dp, 1e153_dp, 1e154_dp, 1e155_dp, 1e156_dp, 1e157_dp, 1e158_dp, &
        1e159_dp, 1e160_dp, 1e161_dp, 1e162_dp, 1e163_dp, 1e164_dp, 1e165_dp, &
        1e166_dp, 1e167_dp, 1e168_dp, 1e169_dp, 1e170_dp, 1e171_dp, 1e172_dp, &
        1e173_dp, 1e174_dp, 1e175_dp, 1e176_dp, 1e177_dp, 1e178_dp, 1e179_dp, &
        1e180_dp, 1e181_dp, 1e182_dp, 1e183_dp, 1e184_dp, 1e185_dp, 1e186_dp, &
        1e187_dp, 1e188_dp, 1e189_dp, 1e190_dp, 1e191_dp, 1e192_dp, 1e193_dp, &
        1e194_dp, 1e195_dp, 1e196_dp, 1e197_dp, 1e198_dp, 1e199_dp, 1e200_dp, &
        1e201_dp, 1e202_dp, 1e203_dp, 1e204_dp, 1e205_dp, 1e206_dp, 1e207_dp, &
        1e208_dp, 1e209_dp, 1e210_dp, 1e211_dp, 1e212_dp, 1e213_dp, 1e214_dp, &
        1e215_dp, 1e216_dp, 1e217_dp, 1e218_dp, 1e219_dp, 1e220_dp, 1e221_dp, &
        1e222_dp, 1e223_dp, 1e224_dp, 1e225_dp, 1e226_dp, 1e227_dp, 1e228_dp, &
        1e229_dp, 1e230_dp, 1e231_dp, 1e232_dp, 1e233_dp, 1e234_dp, 1e235_dp, &
        1e236_dp, 1e237_dp, 1e238_dp, 1e239_dp, 1e240_dp, 1e241_dp, 1e242_dp, &
        1e243_dp, 1e244_dp, 1e245_dp, 1e246_dp, 1e247_dp, 1e248_dp, 1e249_dp, &
        1e250_dp, 1e251_dp, 1e252_dp, 1e253_dp, 1e254_dp, 1e255_dp, 1e256_dp, &
        1e257_dp, 1e258_dp, 1e259_dp, 1e260_dp, 1e261_dp, 1e262_dp, 1e263_dp, &
        1e264_dp, 1e265_dp, 1e266_dp, 1e267_dp, 1e268_dp, 1e269_dp, 1e270_dp, &
        1e271_dp, 1e272_dp, 1e273_dp, 1e274_dp, 1e275_dp, 1e276_dp, 1e277_dp, &
        1e278_dp, 1e279_dp, 1e280_dp, 1e281_dp, 1e282_dp, 1e283_dp, 1e284_dp, &
        1e285_dp, 1e286_dp, 1e287_dp, 1e288_dp, 1e289_dp, 1e290_dp, 1e291_dp, &
        1e292_dp, 1e293_dp, 1e294_dp, 1e295_dp, 1e296_dp, 1e297_dp, 1e298_dp, &
        1e299_dp, 1e300_dp, 1e301_dp, 1e302_dp, 1e303_dp, 1e304_dp, 1e305_dp, &
        1e306_dp, 1e307_dp, 1e308_dp, &
        REAL_DP_INF &
    ]
#endif
#ifdef _XDP
    real(XDP), parameter, public :: &
        REAL_XDP_INF = _REAL_XDP_INF, &
        REAL_XDP_NAN = _REAL_XDP_NAN
    real(XDP), parameter :: REAL_XDP_DEC_POWERS1(*) = [ &
        0._xdp, tiny(0._xdp) * epsilon(0._xdp), &
        1e-4950_xdp, 1e-4949_xdp, 1e-4948_xdp, 1e-4947_xdp, 1e-4946_xdp, &
        1e-4945_xdp, 1e-4944_xdp, 1e-4943_xdp, 1e-4942_xdp, 1e-4941_xdp, &
        1e-4940_xdp, 1e-4939_xdp, 1e-4938_xdp, 1e-4937_xdp, 1e-4936_xdp, &
        1e-4935_xdp, 1e-4934_xdp, 1e-4933_xdp, 1e-4932_xdp, 1e-4931_xdp, &
        1e-4930_xdp, 1e-4929_xdp, 1e-4928_xdp, 1e-4927_xdp, 1e-4926_xdp, &
        1e-4925_xdp, 1e-4924_xdp, 1e-4923_xdp, 1e-4922_xdp, 1e-4921_xdp, &
        1e-4920_xdp, 1e-4919_xdp, 1e-4918_xdp, 1e-4917_xdp, 1e-4916_xdp, &
        1e-4915_xdp, 1e-4914_xdp, 1e-4913_xdp, 1e-4912_xdp, 1e-4911_xdp, &
        1e-4910_xdp, 1e-4909_xdp, 1e-4908_xdp, 1e-4907_xdp, 1e-4906_xdp, &
        1e-4905_xdp, 1e-4904_xdp, 1e-4903_xdp, 1e-4902_xdp, 1e-4901_xdp, &
        1e-4900_xdp, 1e-4899_xdp, 1e-4898_xdp, 1e-4897_xdp, 1e-4896_xdp, &
        1e-4895_xdp, 1e-4894_xdp, 1e-4893_xdp, 1e-4892_xdp, 1e-4891_xdp, &
        1e-4890_xdp, 1e-4889_xdp, 1e-4888_xdp, 1e-4887_xdp, 1e-4886_xdp, &
        1e-4885_xdp, 1e-4884_xdp, 1e-4883_xdp, 1e-4882_xdp, 1e-4881_xdp, &
        1e-4880_xdp, 1e-4879_xdp, 1e-4878_xdp, 1e-4877_xdp, 1e-4876_xdp, &
        1e-4875_xdp, 1e-4874_xdp, 1e-4873_xdp, 1e-4872_xdp, 1e-4871_xdp, &
        1e-4870_xdp, 1e-4869_xdp, 1e-4868_xdp, 1e-4867_xdp, 1e-4866_xdp, &
        1e-4865_xdp, 1e-4864_xdp, 1e-4863_xdp, 1e-4862_xdp, 1e-4861_xdp, &
        1e-4860_xdp, 1e-4859_xdp, 1e-4858_xdp, 1e-4857_xdp, 1e-4856_xdp, &
        1e-4855_xdp, 1e-4854_xdp, 1e-4853_xdp, 1e-4852_xdp, 1e-4851_xdp, &
        1e-4850_xdp, 1e-4849_xdp, 1e-4848_xdp, 1e-4847_xdp, 1e-4846_xdp, &
        1e-4845_xdp, 1e-4844_xdp, 1e-4843_xdp, 1e-4842_xdp, 1e-4841_xdp, &
        1e-4840_xdp, 1e-4839_xdp, 1e-4838_xdp, 1e-4837_xdp, 1e-4836_xdp, &
        1e-4835_xdp, 1e-4834_xdp, 1e-4833_xdp, 1e-4832_xdp, 1e-4831_xdp, &
        1e-4830_xdp, 1e-4829_xdp, 1e-4828_xdp, 1e-4827_xdp, 1e-4826_xdp, &
        1e-4825_xdp, 1e-4824_xdp, 1e-4823_xdp, 1e-4822_xdp, 1e-4821_xdp, &
        1e-4820_xdp, 1e-4819_xdp, 1e-4818_xdp, 1e-4817_xdp, 1e-4816_xdp, &
        1e-4815_xdp, 1e-4814_xdp, 1e-4813_xdp, 1e-4812_xdp, 1e-4811_xdp, &
        1e-4810_xdp, 1e-4809_xdp, 1e-4808_xdp, 1e-4807_xdp, 1e-4806_xdp, &
        1e-4805_xdp, 1e-4804_xdp, 1e-4803_xdp, 1e-4802_xdp, 1e-4801_xdp, &
        1e-4800_xdp, 1e-4799_xdp, 1e-4798_xdp, 1e-4797_xdp, 1e-4796_xdp, &
        1e-4795_xdp, 1e-4794_xdp, 1e-4793_xdp, 1e-4792_xdp, 1e-4791_xdp, &
        1e-4790_xdp, 1e-4789_xdp, 1e-4788_xdp, 1e-4787_xdp, 1e-4786_xdp, &
        1e-4785_xdp, 1e-4784_xdp, 1e-4783_xdp, 1e-4782_xdp, 1e-4781_xdp, &
        1e-4780_xdp, 1e-4779_xdp, 1e-4778_xdp, 1e-4777_xdp, 1e-4776_xdp, &
        1e-4775_xdp, 1e-4774_xdp, 1e-4773_xdp, 1e-4772_xdp, 1e-4771_xdp, &
        1e-4770_xdp, 1e-4769_xdp, 1e-4768_xdp, 1e-4767_xdp, 1e-4766_xdp, &
        1e-4765_xdp, 1e-4764_xdp, 1e-4763_xdp, 1e-4762_xdp, 1e-4761_xdp, &
        1e-4760_xdp, 1e-4759_xdp, 1e-4758_xdp, 1e-4757_xdp, 1e-4756_xdp, &
        1e-4755_xdp, 1e-4754_xdp, 1e-4753_xdp, 1e-4752_xdp, 1e-4751_xdp, &
        1e-4750_xdp, 1e-4749_xdp, 1e-4748_xdp, 1e-4747_xdp, 1e-4746_xdp, &
        1e-4745_xdp, 1e-4744_xdp, 1e-4743_xdp, 1e-4742_xdp, 1e-4741_xdp, &
        1e-4740_xdp, 1e-4739_xdp, 1e-4738_xdp, 1e-4737_xdp, 1e-4736_xdp, &
        1e-4735_xdp, 1e-4734_xdp, 1e-4733_xdp, 1e-4732_xdp, 1e-4731_xdp, &
        1e-4730_xdp, 1e-4729_xdp, 1e-4728_xdp, 1e-4727_xdp, 1e-4726_xdp, &
        1e-4725_xdp, 1e-4724_xdp, 1e-4723_xdp, 1e-4722_xdp, 1e-4721_xdp, &
        1e-4720_xdp, 1e-4719_xdp, 1e-4718_xdp, 1e-4717_xdp, 1e-4716_xdp, &
        1e-4715_xdp, 1e-4714_xdp, 1e-4713_xdp, 1e-4712_xdp, 1e-4711_xdp, &
        1e-4710_xdp, 1e-4709_xdp, 1e-4708_xdp, 1e-4707_xdp, 1e-4706_xdp, &
        1e-4705_xdp, 1e-4704_xdp, 1e-4703_xdp, 1e-4702_xdp, 1e-4701_xdp, &
        1e-4700_xdp, 1e-4699_xdp, 1e-4698_xdp, 1e-4697_xdp, 1e-4696_xdp, &
        1e-4695_xdp, 1e-4694_xdp, 1e-4693_xdp, 1e-4692_xdp, 1e-4691_xdp, &
        1e-4690_xdp, 1e-4689_xdp, 1e-4688_xdp, 1e-4687_xdp, 1e-4686_xdp, &
        1e-4685_xdp, 1e-4684_xdp, 1e-4683_xdp, 1e-4682_xdp, 1e-4681_xdp, &
        1e-4680_xdp, 1e-4679_xdp, 1e-4678_xdp, 1e-4677_xdp, 1e-4676_xdp, &
        1e-4675_xdp, 1e-4674_xdp, 1e-4673_xdp, 1e-4672_xdp, 1e-4671_xdp, &
        1e-4670_xdp, 1e-4669_xdp, 1e-4668_xdp, 1e-4667_xdp, 1e-4666_xdp, &
        1e-4665_xdp, 1e-4664_xdp, 1e-4663_xdp, 1e-4662_xdp, 1e-4661_xdp, &
        1e-4660_xdp, 1e-4659_xdp, 1e-4658_xdp, 1e-4657_xdp, 1e-4656_xdp, &
        1e-4655_xdp, 1e-4654_xdp, 1e-4653_xdp, 1e-4652_xdp, 1e-4651_xdp, &
        1e-4650_xdp, 1e-4649_xdp, 1e-4648_xdp, 1e-4647_xdp, 1e-4646_xdp, &
        1e-4645_xdp, 1e-4644_xdp, 1e-4643_xdp, 1e-4642_xdp, 1e-4641_xdp, &
        1e-4640_xdp, 1e-4639_xdp, 1e-4638_xdp, 1e-4637_xdp, 1e-4636_xdp, &
        1e-4635_xdp, 1e-4634_xdp, 1e-4633_xdp, 1e-4632_xdp, 1e-4631_xdp, &
        1e-4630_xdp, 1e-4629_xdp, 1e-4628_xdp, 1e-4627_xdp, 1e-4626_xdp, &
        1e-4625_xdp, 1e-4624_xdp, 1e-4623_xdp, 1e-4622_xdp, 1e-4621_xdp, &
        1e-4620_xdp, 1e-4619_xdp, 1e-4618_xdp, 1e-4617_xdp, 1e-4616_xdp, &
        1e-4615_xdp, 1e-4614_xdp, 1e-4613_xdp, 1e-4612_xdp, 1e-4611_xdp, &
        1e-4610_xdp, 1e-4609_xdp, 1e-4608_xdp, 1e-4607_xdp, 1e-4606_xdp, &
        1e-4605_xdp, 1e-4604_xdp, 1e-4603_xdp, 1e-4602_xdp, 1e-4601_xdp, &
        1e-4600_xdp, 1e-4599_xdp, 1e-4598_xdp, 1e-4597_xdp, 1e-4596_xdp, &
        1e-4595_xdp, 1e-4594_xdp, 1e-4593_xdp, 1e-4592_xdp, 1e-4591_xdp, &
        1e-4590_xdp, 1e-4589_xdp, 1e-4588_xdp, 1e-4587_xdp, 1e-4586_xdp, &
        1e-4585_xdp, 1e-4584_xdp, 1e-4583_xdp, 1e-4582_xdp, 1e-4581_xdp, &
        1e-4580_xdp, 1e-4579_xdp, 1e-4578_xdp, 1e-4577_xdp, 1e-4576_xdp, &
        1e-4575_xdp, 1e-4574_xdp, 1e-4573_xdp, 1e-4572_xdp, 1e-4571_xdp, &
        1e-4570_xdp, 1e-4569_xdp, 1e-4568_xdp, 1e-4567_xdp, 1e-4566_xdp, &
        1e-4565_xdp, 1e-4564_xdp, 1e-4563_xdp, 1e-4562_xdp, 1e-4561_xdp, &
        1e-4560_xdp, 1e-4559_xdp, 1e-4558_xdp, 1e-4557_xdp, 1e-4556_xdp, &
        1e-4555_xdp, 1e-4554_xdp, 1e-4553_xdp, 1e-4552_xdp, 1e-4551_xdp, &
        1e-4550_xdp, 1e-4549_xdp, 1e-4548_xdp, 1e-4547_xdp, 1e-4546_xdp, &
        1e-4545_xdp, 1e-4544_xdp, 1e-4543_xdp, 1e-4542_xdp, 1e-4541_xdp, &
        1e-4540_xdp, 1e-4539_xdp, 1e-4538_xdp, 1e-4537_xdp, 1e-4536_xdp, &
        1e-4535_xdp, 1e-4534_xdp, 1e-4533_xdp, 1e-4532_xdp, 1e-4531_xdp, &
        1e-4530_xdp, 1e-4529_xdp, 1e-4528_xdp, 1e-4527_xdp, 1e-4526_xdp, &
        1e-4525_xdp, 1e-4524_xdp, 1e-4523_xdp, 1e-4522_xdp, 1e-4521_xdp, &
        1e-4520_xdp, 1e-4519_xdp, 1e-4518_xdp, 1e-4517_xdp, 1e-4516_xdp, &
        1e-4515_xdp, 1e-4514_xdp, 1e-4513_xdp, 1e-4512_xdp, 1e-4511_xdp, &
        1e-4510_xdp, 1e-4509_xdp, 1e-4508_xdp, 1e-4507_xdp, 1e-4506_xdp, &
        1e-4505_xdp, 1e-4504_xdp, 1e-4503_xdp, 1e-4502_xdp, 1e-4501_xdp, &
        1e-4500_xdp, 1e-4499_xdp, 1e-4498_xdp, 1e-4497_xdp, 1e-4496_xdp, &
        1e-4495_xdp, 1e-4494_xdp, 1e-4493_xdp, 1e-4492_xdp, 1e-4491_xdp, &
        1e-4490_xdp, 1e-4489_xdp, 1e-4488_xdp, 1e-4487_xdp, 1e-4486_xdp, &
        1e-4485_xdp, 1e-4484_xdp, 1e-4483_xdp, 1e-4482_xdp, 1e-4481_xdp, &
        1e-4480_xdp, 1e-4479_xdp, 1e-4478_xdp, 1e-4477_xdp, 1e-4476_xdp, &
        1e-4475_xdp, 1e-4474_xdp, 1e-4473_xdp, 1e-4472_xdp, 1e-4471_xdp, &
        1e-4470_xdp, 1e-4469_xdp, 1e-4468_xdp, 1e-4467_xdp, 1e-4466_xdp, &
        1e-4465_xdp, 1e-4464_xdp, 1e-4463_xdp, 1e-4462_xdp, 1e-4461_xdp, &
        1e-4460_xdp, 1e-4459_xdp, 1e-4458_xdp, 1e-4457_xdp, 1e-4456_xdp, &
        1e-4455_xdp, 1e-4454_xdp, 1e-4453_xdp, 1e-4452_xdp, 1e-4451_xdp, &
        1e-4450_xdp, 1e-4449_xdp, 1e-4448_xdp, 1e-4447_xdp, 1e-4446_xdp, &
        1e-4445_xdp, 1e-4444_xdp, 1e-4443_xdp, 1e-4442_xdp, 1e-4441_xdp, &
        1e-4440_xdp, 1e-4439_xdp, 1e-4438_xdp, 1e-4437_xdp, 1e-4436_xdp, &
        1e-4435_xdp, 1e-4434_xdp, 1e-4433_xdp, 1e-4432_xdp, 1e-4431_xdp, &
        1e-4430_xdp, 1e-4429_xdp, 1e-4428_xdp, 1e-4427_xdp, 1e-4426_xdp, &
        1e-4425_xdp, 1e-4424_xdp, 1e-4423_xdp, 1e-4422_xdp, 1e-4421_xdp, &
        1e-4420_xdp, 1e-4419_xdp, 1e-4418_xdp, 1e-4417_xdp, 1e-4416_xdp, &
        1e-4415_xdp, 1e-4414_xdp, 1e-4413_xdp, 1e-4412_xdp, 1e-4411_xdp, &
        1e-4410_xdp, 1e-4409_xdp, 1e-4408_xdp, 1e-4407_xdp, 1e-4406_xdp, &
        1e-4405_xdp, 1e-4404_xdp, 1e-4403_xdp, 1e-4402_xdp, 1e-4401_xdp, &
        1e-4400_xdp, 1e-4399_xdp, 1e-4398_xdp, 1e-4397_xdp, 1e-4396_xdp, &
        1e-4395_xdp, 1e-4394_xdp, 1e-4393_xdp, 1e-4392_xdp, 1e-4391_xdp, &
        1e-4390_xdp, 1e-4389_xdp, 1e-4388_xdp, 1e-4387_xdp, 1e-4386_xdp, &
        1e-4385_xdp, 1e-4384_xdp, 1e-4383_xdp, 1e-4382_xdp, 1e-4381_xdp, &
        1e-4380_xdp, 1e-4379_xdp, 1e-4378_xdp, 1e-4377_xdp, 1e-4376_xdp, &
        1e-4375_xdp, 1e-4374_xdp, 1e-4373_xdp, 1e-4372_xdp, 1e-4371_xdp, &
        1e-4370_xdp, 1e-4369_xdp, 1e-4368_xdp, 1e-4367_xdp, 1e-4366_xdp, &
        1e-4365_xdp, 1e-4364_xdp, 1e-4363_xdp, 1e-4362_xdp, 1e-4361_xdp, &
        1e-4360_xdp, 1e-4359_xdp, 1e-4358_xdp, 1e-4357_xdp, 1e-4356_xdp, &
        1e-4355_xdp, 1e-4354_xdp, 1e-4353_xdp, 1e-4352_xdp, 1e-4351_xdp, &
        1e-4350_xdp, 1e-4349_xdp, 1e-4348_xdp, 1e-4347_xdp, 1e-4346_xdp, &
        1e-4345_xdp, 1e-4344_xdp, 1e-4343_xdp, 1e-4342_xdp, 1e-4341_xdp, &
        1e-4340_xdp, 1e-4339_xdp, 1e-4338_xdp, 1e-4337_xdp, 1e-4336_xdp, &
        1e-4335_xdp, 1e-4334_xdp, 1e-4333_xdp, 1e-4332_xdp, 1e-4331_xdp, &
        1e-4330_xdp, 1e-4329_xdp, 1e-4328_xdp, 1e-4327_xdp, 1e-4326_xdp, &
        1e-4325_xdp, 1e-4324_xdp, 1e-4323_xdp, 1e-4322_xdp, 1e-4321_xdp, &
        1e-4320_xdp, 1e-4319_xdp, 1e-4318_xdp, 1e-4317_xdp, 1e-4316_xdp, &
        1e-4315_xdp, 1e-4314_xdp, 1e-4313_xdp, 1e-4312_xdp, 1e-4311_xdp, &
        1e-4310_xdp, 1e-4309_xdp, 1e-4308_xdp, 1e-4307_xdp, 1e-4306_xdp, &
        1e-4305_xdp, 1e-4304_xdp, 1e-4303_xdp, 1e-4302_xdp, 1e-4301_xdp, &
        1e-4300_xdp, 1e-4299_xdp, 1e-4298_xdp, 1e-4297_xdp, 1e-4296_xdp, &
        1e-4295_xdp, 1e-4294_xdp, 1e-4293_xdp, 1e-4292_xdp, 1e-4291_xdp, &
        1e-4290_xdp, 1e-4289_xdp, 1e-4288_xdp, 1e-4287_xdp, 1e-4286_xdp, &
        1e-4285_xdp, 1e-4284_xdp, 1e-4283_xdp, 1e-4282_xdp, 1e-4281_xdp, &
        1e-4280_xdp, 1e-4279_xdp, 1e-4278_xdp, 1e-4277_xdp, 1e-4276_xdp, &
        1e-4275_xdp, 1e-4274_xdp, 1e-4273_xdp, 1e-4272_xdp, 1e-4271_xdp, &
        1e-4270_xdp, 1e-4269_xdp, 1e-4268_xdp, 1e-4267_xdp, 1e-4266_xdp, &
        1e-4265_xdp, 1e-4264_xdp, 1e-4263_xdp, 1e-4262_xdp, 1e-4261_xdp, &
        1e-4260_xdp, 1e-4259_xdp, 1e-4258_xdp, 1e-4257_xdp, 1e-4256_xdp, &
        1e-4255_xdp, 1e-4254_xdp, 1e-4253_xdp, 1e-4252_xdp, 1e-4251_xdp, &
        1e-4250_xdp, 1e-4249_xdp, 1e-4248_xdp, 1e-4247_xdp, 1e-4246_xdp, &
        1e-4245_xdp, 1e-4244_xdp, 1e-4243_xdp, 1e-4242_xdp, 1e-4241_xdp, &
        1e-4240_xdp, 1e-4239_xdp, 1e-4238_xdp, 1e-4237_xdp, 1e-4236_xdp, &
        1e-4235_xdp, 1e-4234_xdp, 1e-4233_xdp, 1e-4232_xdp, 1e-4231_xdp, &
        1e-4230_xdp, 1e-4229_xdp, 1e-4228_xdp, 1e-4227_xdp, 1e-4226_xdp, &
        1e-4225_xdp, 1e-4224_xdp, 1e-4223_xdp, 1e-4222_xdp, 1e-4221_xdp, &
        1e-4220_xdp, 1e-4219_xdp, 1e-4218_xdp, 1e-4217_xdp, 1e-4216_xdp, &
        1e-4215_xdp, 1e-4214_xdp, 1e-4213_xdp, 1e-4212_xdp, 1e-4211_xdp, &
        1e-4210_xdp, 1e-4209_xdp, 1e-4208_xdp, 1e-4207_xdp, 1e-4206_xdp, &
        1e-4205_xdp, 1e-4204_xdp, 1e-4203_xdp, 1e-4202_xdp, 1e-4201_xdp, &
        1e-4200_xdp, 1e-4199_xdp, 1e-4198_xdp, 1e-4197_xdp, 1e-4196_xdp, &
        1e-4195_xdp, 1e-4194_xdp, 1e-4193_xdp, 1e-4192_xdp, 1e-4191_xdp, &
        1e-4190_xdp, 1e-4189_xdp, 1e-4188_xdp, 1e-4187_xdp, 1e-4186_xdp, &
        1e-4185_xdp, 1e-4184_xdp, 1e-4183_xdp, 1e-4182_xdp, 1e-4181_xdp, &
        1e-4180_xdp, 1e-4179_xdp, 1e-4178_xdp, 1e-4177_xdp, 1e-4176_xdp, &
        1e-4175_xdp, 1e-4174_xdp, 1e-4173_xdp, 1e-4172_xdp, 1e-4171_xdp, &
        1e-4170_xdp, 1e-4169_xdp, 1e-4168_xdp, 1e-4167_xdp, 1e-4166_xdp, &
        1e-4165_xdp, 1e-4164_xdp, 1e-4163_xdp, 1e-4162_xdp, 1e-4161_xdp, &
        1e-4160_xdp, 1e-4159_xdp, 1e-4158_xdp, 1e-4157_xdp, 1e-4156_xdp, &
        1e-4155_xdp, 1e-4154_xdp, 1e-4153_xdp, 1e-4152_xdp, 1e-4151_xdp, &
        1e-4150_xdp, 1e-4149_xdp, 1e-4148_xdp, 1e-4147_xdp, 1e-4146_xdp, &
        1e-4145_xdp, 1e-4144_xdp, 1e-4143_xdp, 1e-4142_xdp, 1e-4141_xdp, &
        1e-4140_xdp, 1e-4139_xdp, 1e-4138_xdp, 1e-4137_xdp, 1e-4136_xdp, &
        1e-4135_xdp, 1e-4134_xdp, 1e-4133_xdp, 1e-4132_xdp, 1e-4131_xdp, &
        1e-4130_xdp, 1e-4129_xdp, 1e-4128_xdp, 1e-4127_xdp, 1e-4126_xdp, &
        1e-4125_xdp, 1e-4124_xdp, 1e-4123_xdp, 1e-4122_xdp, 1e-4121_xdp, &
        1e-4120_xdp, 1e-4119_xdp, 1e-4118_xdp, 1e-4117_xdp, 1e-4116_xdp, &
        1e-4115_xdp, 1e-4114_xdp, 1e-4113_xdp, 1e-4112_xdp, 1e-4111_xdp, &
        1e-4110_xdp, 1e-4109_xdp, 1e-4108_xdp, 1e-4107_xdp, 1e-4106_xdp, &
        1e-4105_xdp, 1e-4104_xdp, 1e-4103_xdp, 1e-4102_xdp, 1e-4101_xdp, &
        1e-4100_xdp, 1e-4099_xdp, 1e-4098_xdp, 1e-4097_xdp, 1e-4096_xdp, &
        1e-4095_xdp, 1e-4094_xdp, 1e-4093_xdp, 1e-4092_xdp, 1e-4091_xdp, &
        1e-4090_xdp, 1e-4089_xdp, 1e-4088_xdp, 1e-4087_xdp, 1e-4086_xdp, &
        1e-4085_xdp, 1e-4084_xdp, 1e-4083_xdp, 1e-4082_xdp, 1e-4081_xdp, &
        1e-4080_xdp, 1e-4079_xdp, 1e-4078_xdp, 1e-4077_xdp, 1e-4076_xdp, &
        1e-4075_xdp, 1e-4074_xdp, 1e-4073_xdp, 1e-4072_xdp, 1e-4071_xdp, &
        1e-4070_xdp, 1e-4069_xdp, 1e-4068_xdp, 1e-4067_xdp, 1e-4066_xdp, &
        1e-4065_xdp, 1e-4064_xdp, 1e-4063_xdp, 1e-4062_xdp, 1e-4061_xdp, &
        1e-4060_xdp, 1e-4059_xdp, 1e-4058_xdp, 1e-4057_xdp, 1e-4056_xdp, &
        1e-4055_xdp, 1e-4054_xdp, 1e-4053_xdp, 1e-4052_xdp, 1e-4051_xdp, &
        1e-4050_xdp, 1e-4049_xdp, 1e-4048_xdp, 1e-4047_xdp, 1e-4046_xdp, &
        1e-4045_xdp, 1e-4044_xdp, 1e-4043_xdp, 1e-4042_xdp, 1e-4041_xdp, &
        1e-4040_xdp, 1e-4039_xdp, 1e-4038_xdp, 1e-4037_xdp, 1e-4036_xdp, &
        1e-4035_xdp, 1e-4034_xdp, 1e-4033_xdp, 1e-4032_xdp, 1e-4031_xdp, &
        1e-4030_xdp, 1e-4029_xdp, 1e-4028_xdp, 1e-4027_xdp, 1e-4026_xdp, &
        1e-4025_xdp, 1e-4024_xdp, 1e-4023_xdp, 1e-4022_xdp, 1e-4021_xdp, &
        1e-4020_xdp, 1e-4019_xdp, 1e-4018_xdp, 1e-4017_xdp, 1e-4016_xdp, &
        1e-4015_xdp, 1e-4014_xdp, 1e-4013_xdp, 1e-4012_xdp, 1e-4011_xdp, &
        1e-4010_xdp, 1e-4009_xdp, 1e-4008_xdp, 1e-4007_xdp, 1e-4006_xdp, &
        1e-4005_xdp, 1e-4004_xdp, 1e-4003_xdp, 1e-4002_xdp, 1e-4001_xdp, &
        1e-4000_xdp, 1e-3999_xdp, 1e-3998_xdp, 1e-3997_xdp, 1e-3996_xdp, &
        1e-3995_xdp, 1e-3994_xdp, 1e-3993_xdp, 1e-3992_xdp, 1e-3991_xdp, &
        1e-3990_xdp, 1e-3989_xdp, 1e-3988_xdp, 1e-3987_xdp, 1e-3986_xdp, &
        1e-3985_xdp, 1e-3984_xdp, 1e-3983_xdp, 1e-3982_xdp, 1e-3981_xdp, &
        1e-3980_xdp, 1e-3979_xdp, 1e-3978_xdp, 1e-3977_xdp, 1e-3976_xdp, &
        1e-3975_xdp, 1e-3974_xdp, 1e-3973_xdp, 1e-3972_xdp, 1e-3971_xdp, &
        1e-3970_xdp, 1e-3969_xdp, 1e-3968_xdp, 1e-3967_xdp, 1e-3966_xdp, &
        1e-3965_xdp, 1e-3964_xdp, 1e-3963_xdp, 1e-3962_xdp, 1e-3961_xdp, &
        1e-3960_xdp, 1e-3959_xdp, 1e-3958_xdp, 1e-3957_xdp, 1e-3956_xdp, &
        1e-3955_xdp, 1e-3954_xdp, 1e-3953_xdp, 1e-3952_xdp, 1e-3951_xdp, &
        1e-3950_xdp, 1e-3949_xdp, 1e-3948_xdp, 1e-3947_xdp, 1e-3946_xdp, &
        1e-3945_xdp, 1e-3944_xdp, 1e-3943_xdp, 1e-3942_xdp, 1e-3941_xdp, &
        1e-3940_xdp, 1e-3939_xdp, 1e-3938_xdp, 1e-3937_xdp, 1e-3936_xdp, &
        1e-3935_xdp, 1e-3934_xdp, 1e-3933_xdp, 1e-3932_xdp, 1e-3931_xdp, &
        1e-3930_xdp, 1e-3929_xdp, 1e-3928_xdp, 1e-3927_xdp, 1e-3926_xdp, &
        1e-3925_xdp, 1e-3924_xdp, 1e-3923_xdp, 1e-3922_xdp, 1e-3921_xdp, &
        1e-3920_xdp, 1e-3919_xdp, 1e-3918_xdp, 1e-3917_xdp, 1e-3916_xdp, &
        1e-3915_xdp, 1e-3914_xdp, 1e-3913_xdp, 1e-3912_xdp, 1e-3911_xdp, &
        1e-3910_xdp, 1e-3909_xdp, 1e-3908_xdp, 1e-3907_xdp, 1e-3906_xdp, &
        1e-3905_xdp, 1e-3904_xdp, 1e-3903_xdp, 1e-3902_xdp, 1e-3901_xdp, &
        1e-3900_xdp, 1e-3899_xdp, 1e-3898_xdp, 1e-3897_xdp, 1e-3896_xdp, &
        1e-3895_xdp, 1e-3894_xdp, 1e-3893_xdp, 1e-3892_xdp, 1e-3891_xdp, &
        1e-3890_xdp, 1e-3889_xdp, 1e-3888_xdp, 1e-3887_xdp, 1e-3886_xdp, &
        1e-3885_xdp, 1e-3884_xdp, 1e-3883_xdp, 1e-3882_xdp, 1e-3881_xdp, &
        1e-3880_xdp, 1e-3879_xdp, 1e-3878_xdp, 1e-3877_xdp, 1e-3876_xdp, &
        1e-3875_xdp, 1e-3874_xdp, 1e-3873_xdp, 1e-3872_xdp, 1e-3871_xdp, &
        1e-3870_xdp, 1e-3869_xdp, 1e-3868_xdp, 1e-3867_xdp, 1e-3866_xdp, &
        1e-3865_xdp, 1e-3864_xdp, 1e-3863_xdp, 1e-3862_xdp, 1e-3861_xdp, &
        1e-3860_xdp, 1e-3859_xdp, 1e-3858_xdp, 1e-3857_xdp, 1e-3856_xdp, &
        1e-3855_xdp, 1e-3854_xdp, 1e-3853_xdp, 1e-3852_xdp, 1e-3851_xdp, &
        1e-3850_xdp, 1e-3849_xdp, 1e-3848_xdp, 1e-3847_xdp, 1e-3846_xdp, &
        1e-3845_xdp, 1e-3844_xdp, 1e-3843_xdp, 1e-3842_xdp, 1e-3841_xdp, &
        1e-3840_xdp, 1e-3839_xdp, 1e-3838_xdp, 1e-3837_xdp, 1e-3836_xdp, &
        1e-3835_xdp, 1e-3834_xdp, 1e-3833_xdp, 1e-3832_xdp, 1e-3831_xdp, &
        1e-3830_xdp, 1e-3829_xdp, 1e-3828_xdp, 1e-3827_xdp, 1e-3826_xdp, &
        1e-3825_xdp, 1e-3824_xdp, 1e-3823_xdp, 1e-3822_xdp, 1e-3821_xdp, &
        1e-3820_xdp, 1e-3819_xdp, 1e-3818_xdp, 1e-3817_xdp, 1e-3816_xdp, &
        1e-3815_xdp, 1e-3814_xdp, 1e-3813_xdp, 1e-3812_xdp, 1e-3811_xdp, &
        1e-3810_xdp, 1e-3809_xdp, 1e-3808_xdp, 1e-3807_xdp, 1e-3806_xdp, &
        1e-3805_xdp, 1e-3804_xdp, 1e-3803_xdp, 1e-3802_xdp, 1e-3801_xdp, &
        1e-3800_xdp, 1e-3799_xdp, 1e-3798_xdp, 1e-3797_xdp, 1e-3796_xdp, &
        1e-3795_xdp, 1e-3794_xdp, 1e-3793_xdp, 1e-3792_xdp, 1e-3791_xdp, &
        1e-3790_xdp, 1e-3789_xdp, 1e-3788_xdp, 1e-3787_xdp, 1e-3786_xdp, &
        1e-3785_xdp, 1e-3784_xdp, 1e-3783_xdp, 1e-3782_xdp, 1e-3781_xdp, &
        1e-3780_xdp, 1e-3779_xdp, 1e-3778_xdp, 1e-3777_xdp, 1e-3776_xdp, &
        1e-3775_xdp, 1e-3774_xdp, 1e-3773_xdp, 1e-3772_xdp, 1e-3771_xdp, &
        1e-3770_xdp, 1e-3769_xdp, 1e-3768_xdp, 1e-3767_xdp, 1e-3766_xdp, &
        1e-3765_xdp, 1e-3764_xdp, 1e-3763_xdp, 1e-3762_xdp, 1e-3761_xdp, &
        1e-3760_xdp, 1e-3759_xdp, 1e-3758_xdp, 1e-3757_xdp, 1e-3756_xdp, &
        1e-3755_xdp, 1e-3754_xdp, 1e-3753_xdp, 1e-3752_xdp, 1e-3751_xdp, &
        1e-3750_xdp, 1e-3749_xdp, 1e-3748_xdp, 1e-3747_xdp, 1e-3746_xdp, &
        1e-3745_xdp, 1e-3744_xdp, 1e-3743_xdp, 1e-3742_xdp, 1e-3741_xdp, &
        1e-3740_xdp, 1e-3739_xdp, 1e-3738_xdp, 1e-3737_xdp, 1e-3736_xdp, &
        1e-3735_xdp, 1e-3734_xdp, 1e-3733_xdp, 1e-3732_xdp, 1e-3731_xdp, &
        1e-3730_xdp, 1e-3729_xdp, 1e-3728_xdp, 1e-3727_xdp, 1e-3726_xdp, &
        1e-3725_xdp, 1e-3724_xdp, 1e-3723_xdp, 1e-3722_xdp, 1e-3721_xdp, &
        1e-3720_xdp, 1e-3719_xdp, 1e-3718_xdp, 1e-3717_xdp, 1e-3716_xdp, &
        1e-3715_xdp, 1e-3714_xdp, 1e-3713_xdp, 1e-3712_xdp, 1e-3711_xdp, &
        1e-3710_xdp, 1e-3709_xdp, 1e-3708_xdp, 1e-3707_xdp, 1e-3706_xdp, &
        1e-3705_xdp, 1e-3704_xdp, 1e-3703_xdp, 1e-3702_xdp, 1e-3701_xdp, &
        1e-3700_xdp, 1e-3699_xdp, 1e-3698_xdp, 1e-3697_xdp, 1e-3696_xdp, &
        1e-3695_xdp, 1e-3694_xdp, 1e-3693_xdp, 1e-3692_xdp, 1e-3691_xdp, &
        1e-3690_xdp, 1e-3689_xdp, 1e-3688_xdp, 1e-3687_xdp, 1e-3686_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS2(*) = [ &
        1e-3685_xdp, 1e-3684_xdp, 1e-3683_xdp, 1e-3682_xdp, 1e-3681_xdp, &
        1e-3680_xdp, 1e-3679_xdp, 1e-3678_xdp, 1e-3677_xdp, 1e-3676_xdp, &
        1e-3675_xdp, 1e-3674_xdp, 1e-3673_xdp, 1e-3672_xdp, 1e-3671_xdp, &
        1e-3670_xdp, 1e-3669_xdp, 1e-3668_xdp, 1e-3667_xdp, 1e-3666_xdp, &
        1e-3665_xdp, 1e-3664_xdp, 1e-3663_xdp, 1e-3662_xdp, 1e-3661_xdp, &
        1e-3660_xdp, 1e-3659_xdp, 1e-3658_xdp, 1e-3657_xdp, 1e-3656_xdp, &
        1e-3655_xdp, 1e-3654_xdp, 1e-3653_xdp, 1e-3652_xdp, 1e-3651_xdp, &
        1e-3650_xdp, 1e-3649_xdp, 1e-3648_xdp, 1e-3647_xdp, 1e-3646_xdp, &
        1e-3645_xdp, 1e-3644_xdp, 1e-3643_xdp, 1e-3642_xdp, 1e-3641_xdp, &
        1e-3640_xdp, 1e-3639_xdp, 1e-3638_xdp, 1e-3637_xdp, 1e-3636_xdp, &
        1e-3635_xdp, 1e-3634_xdp, 1e-3633_xdp, 1e-3632_xdp, 1e-3631_xdp, &
        1e-3630_xdp, 1e-3629_xdp, 1e-3628_xdp, 1e-3627_xdp, 1e-3626_xdp, &
        1e-3625_xdp, 1e-3624_xdp, 1e-3623_xdp, 1e-3622_xdp, 1e-3621_xdp, &
        1e-3620_xdp, 1e-3619_xdp, 1e-3618_xdp, 1e-3617_xdp, 1e-3616_xdp, &
        1e-3615_xdp, 1e-3614_xdp, 1e-3613_xdp, 1e-3612_xdp, 1e-3611_xdp, &
        1e-3610_xdp, 1e-3609_xdp, 1e-3608_xdp, 1e-3607_xdp, 1e-3606_xdp, &
        1e-3605_xdp, 1e-3604_xdp, 1e-3603_xdp, 1e-3602_xdp, 1e-3601_xdp, &
        1e-3600_xdp, 1e-3599_xdp, 1e-3598_xdp, 1e-3597_xdp, 1e-3596_xdp, &
        1e-3595_xdp, 1e-3594_xdp, 1e-3593_xdp, 1e-3592_xdp, 1e-3591_xdp, &
        1e-3590_xdp, 1e-3589_xdp, 1e-3588_xdp, 1e-3587_xdp, 1e-3586_xdp, &
        1e-3585_xdp, 1e-3584_xdp, 1e-3583_xdp, 1e-3582_xdp, 1e-3581_xdp, &
        1e-3580_xdp, 1e-3579_xdp, 1e-3578_xdp, 1e-3577_xdp, 1e-3576_xdp, &
        1e-3575_xdp, 1e-3574_xdp, 1e-3573_xdp, 1e-3572_xdp, 1e-3571_xdp, &
        1e-3570_xdp, 1e-3569_xdp, 1e-3568_xdp, 1e-3567_xdp, 1e-3566_xdp, &
        1e-3565_xdp, 1e-3564_xdp, 1e-3563_xdp, 1e-3562_xdp, 1e-3561_xdp, &
        1e-3560_xdp, 1e-3559_xdp, 1e-3558_xdp, 1e-3557_xdp, 1e-3556_xdp, &
        1e-3555_xdp, 1e-3554_xdp, 1e-3553_xdp, 1e-3552_xdp, 1e-3551_xdp, &
        1e-3550_xdp, 1e-3549_xdp, 1e-3548_xdp, 1e-3547_xdp, 1e-3546_xdp, &
        1e-3545_xdp, 1e-3544_xdp, 1e-3543_xdp, 1e-3542_xdp, 1e-3541_xdp, &
        1e-3540_xdp, 1e-3539_xdp, 1e-3538_xdp, 1e-3537_xdp, 1e-3536_xdp, &
        1e-3535_xdp, 1e-3534_xdp, 1e-3533_xdp, 1e-3532_xdp, 1e-3531_xdp, &
        1e-3530_xdp, 1e-3529_xdp, 1e-3528_xdp, 1e-3527_xdp, 1e-3526_xdp, &
        1e-3525_xdp, 1e-3524_xdp, 1e-3523_xdp, 1e-3522_xdp, 1e-3521_xdp, &
        1e-3520_xdp, 1e-3519_xdp, 1e-3518_xdp, 1e-3517_xdp, 1e-3516_xdp, &
        1e-3515_xdp, 1e-3514_xdp, 1e-3513_xdp, 1e-3512_xdp, 1e-3511_xdp, &
        1e-3510_xdp, 1e-3509_xdp, 1e-3508_xdp, 1e-3507_xdp, 1e-3506_xdp, &
        1e-3505_xdp, 1e-3504_xdp, 1e-3503_xdp, 1e-3502_xdp, 1e-3501_xdp, &
        1e-3500_xdp, 1e-3499_xdp, 1e-3498_xdp, 1e-3497_xdp, 1e-3496_xdp, &
        1e-3495_xdp, 1e-3494_xdp, 1e-3493_xdp, 1e-3492_xdp, 1e-3491_xdp, &
        1e-3490_xdp, 1e-3489_xdp, 1e-3488_xdp, 1e-3487_xdp, 1e-3486_xdp, &
        1e-3485_xdp, 1e-3484_xdp, 1e-3483_xdp, 1e-3482_xdp, 1e-3481_xdp, &
        1e-3480_xdp, 1e-3479_xdp, 1e-3478_xdp, 1e-3477_xdp, 1e-3476_xdp, &
        1e-3475_xdp, 1e-3474_xdp, 1e-3473_xdp, 1e-3472_xdp, 1e-3471_xdp, &
        1e-3470_xdp, 1e-3469_xdp, 1e-3468_xdp, 1e-3467_xdp, 1e-3466_xdp, &
        1e-3465_xdp, 1e-3464_xdp, 1e-3463_xdp, 1e-3462_xdp, 1e-3461_xdp, &
        1e-3460_xdp, 1e-3459_xdp, 1e-3458_xdp, 1e-3457_xdp, 1e-3456_xdp, &
        1e-3455_xdp, 1e-3454_xdp, 1e-3453_xdp, 1e-3452_xdp, 1e-3451_xdp, &
        1e-3450_xdp, 1e-3449_xdp, 1e-3448_xdp, 1e-3447_xdp, 1e-3446_xdp, &
        1e-3445_xdp, 1e-3444_xdp, 1e-3443_xdp, 1e-3442_xdp, 1e-3441_xdp, &
        1e-3440_xdp, 1e-3439_xdp, 1e-3438_xdp, 1e-3437_xdp, 1e-3436_xdp, &
        1e-3435_xdp, 1e-3434_xdp, 1e-3433_xdp, 1e-3432_xdp, 1e-3431_xdp, &
        1e-3430_xdp, 1e-3429_xdp, 1e-3428_xdp, 1e-3427_xdp, 1e-3426_xdp, &
        1e-3425_xdp, 1e-3424_xdp, 1e-3423_xdp, 1e-3422_xdp, 1e-3421_xdp, &
        1e-3420_xdp, 1e-3419_xdp, 1e-3418_xdp, 1e-3417_xdp, 1e-3416_xdp, &
        1e-3415_xdp, 1e-3414_xdp, 1e-3413_xdp, 1e-3412_xdp, 1e-3411_xdp, &
        1e-3410_xdp, 1e-3409_xdp, 1e-3408_xdp, 1e-3407_xdp, 1e-3406_xdp, &
        1e-3405_xdp, 1e-3404_xdp, 1e-3403_xdp, 1e-3402_xdp, 1e-3401_xdp, &
        1e-3400_xdp, 1e-3399_xdp, 1e-3398_xdp, 1e-3397_xdp, 1e-3396_xdp, &
        1e-3395_xdp, 1e-3394_xdp, 1e-3393_xdp, 1e-3392_xdp, 1e-3391_xdp, &
        1e-3390_xdp, 1e-3389_xdp, 1e-3388_xdp, 1e-3387_xdp, 1e-3386_xdp, &
        1e-3385_xdp, 1e-3384_xdp, 1e-3383_xdp, 1e-3382_xdp, 1e-3381_xdp, &
        1e-3380_xdp, 1e-3379_xdp, 1e-3378_xdp, 1e-3377_xdp, 1e-3376_xdp, &
        1e-3375_xdp, 1e-3374_xdp, 1e-3373_xdp, 1e-3372_xdp, 1e-3371_xdp, &
        1e-3370_xdp, 1e-3369_xdp, 1e-3368_xdp, 1e-3367_xdp, 1e-3366_xdp, &
        1e-3365_xdp, 1e-3364_xdp, 1e-3363_xdp, 1e-3362_xdp, 1e-3361_xdp, &
        1e-3360_xdp, 1e-3359_xdp, 1e-3358_xdp, 1e-3357_xdp, 1e-3356_xdp, &
        1e-3355_xdp, 1e-3354_xdp, 1e-3353_xdp, 1e-3352_xdp, 1e-3351_xdp, &
        1e-3350_xdp, 1e-3349_xdp, 1e-3348_xdp, 1e-3347_xdp, 1e-3346_xdp, &
        1e-3345_xdp, 1e-3344_xdp, 1e-3343_xdp, 1e-3342_xdp, 1e-3341_xdp, &
        1e-3340_xdp, 1e-3339_xdp, 1e-3338_xdp, 1e-3337_xdp, 1e-3336_xdp, &
        1e-3335_xdp, 1e-3334_xdp, 1e-3333_xdp, 1e-3332_xdp, 1e-3331_xdp, &
        1e-3330_xdp, 1e-3329_xdp, 1e-3328_xdp, 1e-3327_xdp, 1e-3326_xdp, &
        1e-3325_xdp, 1e-3324_xdp, 1e-3323_xdp, 1e-3322_xdp, 1e-3321_xdp, &
        1e-3320_xdp, 1e-3319_xdp, 1e-3318_xdp, 1e-3317_xdp, 1e-3316_xdp, &
        1e-3315_xdp, 1e-3314_xdp, 1e-3313_xdp, 1e-3312_xdp, 1e-3311_xdp, &
        1e-3310_xdp, 1e-3309_xdp, 1e-3308_xdp, 1e-3307_xdp, 1e-3306_xdp, &
        1e-3305_xdp, 1e-3304_xdp, 1e-3303_xdp, 1e-3302_xdp, 1e-3301_xdp, &
        1e-3300_xdp, 1e-3299_xdp, 1e-3298_xdp, 1e-3297_xdp, 1e-3296_xdp, &
        1e-3295_xdp, 1e-3294_xdp, 1e-3293_xdp, 1e-3292_xdp, 1e-3291_xdp, &
        1e-3290_xdp, 1e-3289_xdp, 1e-3288_xdp, 1e-3287_xdp, 1e-3286_xdp, &
        1e-3285_xdp, 1e-3284_xdp, 1e-3283_xdp, 1e-3282_xdp, 1e-3281_xdp, &
        1e-3280_xdp, 1e-3279_xdp, 1e-3278_xdp, 1e-3277_xdp, 1e-3276_xdp, &
        1e-3275_xdp, 1e-3274_xdp, 1e-3273_xdp, 1e-3272_xdp, 1e-3271_xdp, &
        1e-3270_xdp, 1e-3269_xdp, 1e-3268_xdp, 1e-3267_xdp, 1e-3266_xdp, &
        1e-3265_xdp, 1e-3264_xdp, 1e-3263_xdp, 1e-3262_xdp, 1e-3261_xdp, &
        1e-3260_xdp, 1e-3259_xdp, 1e-3258_xdp, 1e-3257_xdp, 1e-3256_xdp, &
        1e-3255_xdp, 1e-3254_xdp, 1e-3253_xdp, 1e-3252_xdp, 1e-3251_xdp, &
        1e-3250_xdp, 1e-3249_xdp, 1e-3248_xdp, 1e-3247_xdp, 1e-3246_xdp, &
        1e-3245_xdp, 1e-3244_xdp, 1e-3243_xdp, 1e-3242_xdp, 1e-3241_xdp, &
        1e-3240_xdp, 1e-3239_xdp, 1e-3238_xdp, 1e-3237_xdp, 1e-3236_xdp, &
        1e-3235_xdp, 1e-3234_xdp, 1e-3233_xdp, 1e-3232_xdp, 1e-3231_xdp, &
        1e-3230_xdp, 1e-3229_xdp, 1e-3228_xdp, 1e-3227_xdp, 1e-3226_xdp, &
        1e-3225_xdp, 1e-3224_xdp, 1e-3223_xdp, 1e-3222_xdp, 1e-3221_xdp, &
        1e-3220_xdp, 1e-3219_xdp, 1e-3218_xdp, 1e-3217_xdp, 1e-3216_xdp, &
        1e-3215_xdp, 1e-3214_xdp, 1e-3213_xdp, 1e-3212_xdp, 1e-3211_xdp, &
        1e-3210_xdp, 1e-3209_xdp, 1e-3208_xdp, 1e-3207_xdp, 1e-3206_xdp, &
        1e-3205_xdp, 1e-3204_xdp, 1e-3203_xdp, 1e-3202_xdp, 1e-3201_xdp, &
        1e-3200_xdp, 1e-3199_xdp, 1e-3198_xdp, 1e-3197_xdp, 1e-3196_xdp, &
        1e-3195_xdp, 1e-3194_xdp, 1e-3193_xdp, 1e-3192_xdp, 1e-3191_xdp, &
        1e-3190_xdp, 1e-3189_xdp, 1e-3188_xdp, 1e-3187_xdp, 1e-3186_xdp, &
        1e-3185_xdp, 1e-3184_xdp, 1e-3183_xdp, 1e-3182_xdp, 1e-3181_xdp, &
        1e-3180_xdp, 1e-3179_xdp, 1e-3178_xdp, 1e-3177_xdp, 1e-3176_xdp, &
        1e-3175_xdp, 1e-3174_xdp, 1e-3173_xdp, 1e-3172_xdp, 1e-3171_xdp, &
        1e-3170_xdp, 1e-3169_xdp, 1e-3168_xdp, 1e-3167_xdp, 1e-3166_xdp, &
        1e-3165_xdp, 1e-3164_xdp, 1e-3163_xdp, 1e-3162_xdp, 1e-3161_xdp, &
        1e-3160_xdp, 1e-3159_xdp, 1e-3158_xdp, 1e-3157_xdp, 1e-3156_xdp, &
        1e-3155_xdp, 1e-3154_xdp, 1e-3153_xdp, 1e-3152_xdp, 1e-3151_xdp, &
        1e-3150_xdp, 1e-3149_xdp, 1e-3148_xdp, 1e-3147_xdp, 1e-3146_xdp, &
        1e-3145_xdp, 1e-3144_xdp, 1e-3143_xdp, 1e-3142_xdp, 1e-3141_xdp, &
        1e-3140_xdp, 1e-3139_xdp, 1e-3138_xdp, 1e-3137_xdp, 1e-3136_xdp, &
        1e-3135_xdp, 1e-3134_xdp, 1e-3133_xdp, 1e-3132_xdp, 1e-3131_xdp, &
        1e-3130_xdp, 1e-3129_xdp, 1e-3128_xdp, 1e-3127_xdp, 1e-3126_xdp, &
        1e-3125_xdp, 1e-3124_xdp, 1e-3123_xdp, 1e-3122_xdp, 1e-3121_xdp, &
        1e-3120_xdp, 1e-3119_xdp, 1e-3118_xdp, 1e-3117_xdp, 1e-3116_xdp, &
        1e-3115_xdp, 1e-3114_xdp, 1e-3113_xdp, 1e-3112_xdp, 1e-3111_xdp, &
        1e-3110_xdp, 1e-3109_xdp, 1e-3108_xdp, 1e-3107_xdp, 1e-3106_xdp, &
        1e-3105_xdp, 1e-3104_xdp, 1e-3103_xdp, 1e-3102_xdp, 1e-3101_xdp, &
        1e-3100_xdp, 1e-3099_xdp, 1e-3098_xdp, 1e-3097_xdp, 1e-3096_xdp, &
        1e-3095_xdp, 1e-3094_xdp, 1e-3093_xdp, 1e-3092_xdp, 1e-3091_xdp, &
        1e-3090_xdp, 1e-3089_xdp, 1e-3088_xdp, 1e-3087_xdp, 1e-3086_xdp, &
        1e-3085_xdp, 1e-3084_xdp, 1e-3083_xdp, 1e-3082_xdp, 1e-3081_xdp, &
        1e-3080_xdp, 1e-3079_xdp, 1e-3078_xdp, 1e-3077_xdp, 1e-3076_xdp, &
        1e-3075_xdp, 1e-3074_xdp, 1e-3073_xdp, 1e-3072_xdp, 1e-3071_xdp, &
        1e-3070_xdp, 1e-3069_xdp, 1e-3068_xdp, 1e-3067_xdp, 1e-3066_xdp, &
        1e-3065_xdp, 1e-3064_xdp, 1e-3063_xdp, 1e-3062_xdp, 1e-3061_xdp, &
        1e-3060_xdp, 1e-3059_xdp, 1e-3058_xdp, 1e-3057_xdp, 1e-3056_xdp, &
        1e-3055_xdp, 1e-3054_xdp, 1e-3053_xdp, 1e-3052_xdp, 1e-3051_xdp, &
        1e-3050_xdp, 1e-3049_xdp, 1e-3048_xdp, 1e-3047_xdp, 1e-3046_xdp, &
        1e-3045_xdp, 1e-3044_xdp, 1e-3043_xdp, 1e-3042_xdp, 1e-3041_xdp, &
        1e-3040_xdp, 1e-3039_xdp, 1e-3038_xdp, 1e-3037_xdp, 1e-3036_xdp, &
        1e-3035_xdp, 1e-3034_xdp, 1e-3033_xdp, 1e-3032_xdp, 1e-3031_xdp, &
        1e-3030_xdp, 1e-3029_xdp, 1e-3028_xdp, 1e-3027_xdp, 1e-3026_xdp, &
        1e-3025_xdp, 1e-3024_xdp, 1e-3023_xdp, 1e-3022_xdp, 1e-3021_xdp, &
        1e-3020_xdp, 1e-3019_xdp, 1e-3018_xdp, 1e-3017_xdp, 1e-3016_xdp, &
        1e-3015_xdp, 1e-3014_xdp, 1e-3013_xdp, 1e-3012_xdp, 1e-3011_xdp, &
        1e-3010_xdp, 1e-3009_xdp, 1e-3008_xdp, 1e-3007_xdp, 1e-3006_xdp, &
        1e-3005_xdp, 1e-3004_xdp, 1e-3003_xdp, 1e-3002_xdp, 1e-3001_xdp, &
        1e-3000_xdp, 1e-2999_xdp, 1e-2998_xdp, 1e-2997_xdp, 1e-2996_xdp, &
        1e-2995_xdp, 1e-2994_xdp, 1e-2993_xdp, 1e-2992_xdp, 1e-2991_xdp, &
        1e-2990_xdp, 1e-2989_xdp, 1e-2988_xdp, 1e-2987_xdp, 1e-2986_xdp, &
        1e-2985_xdp, 1e-2984_xdp, 1e-2983_xdp, 1e-2982_xdp, 1e-2981_xdp, &
        1e-2980_xdp, 1e-2979_xdp, 1e-2978_xdp, 1e-2977_xdp, 1e-2976_xdp, &
        1e-2975_xdp, 1e-2974_xdp, 1e-2973_xdp, 1e-2972_xdp, 1e-2971_xdp, &
        1e-2970_xdp, 1e-2969_xdp, 1e-2968_xdp, 1e-2967_xdp, 1e-2966_xdp, &
        1e-2965_xdp, 1e-2964_xdp, 1e-2963_xdp, 1e-2962_xdp, 1e-2961_xdp, &
        1e-2960_xdp, 1e-2959_xdp, 1e-2958_xdp, 1e-2957_xdp, 1e-2956_xdp, &
        1e-2955_xdp, 1e-2954_xdp, 1e-2953_xdp, 1e-2952_xdp, 1e-2951_xdp, &
        1e-2950_xdp, 1e-2949_xdp, 1e-2948_xdp, 1e-2947_xdp, 1e-2946_xdp, &
        1e-2945_xdp, 1e-2944_xdp, 1e-2943_xdp, 1e-2942_xdp, 1e-2941_xdp, &
        1e-2940_xdp, 1e-2939_xdp, 1e-2938_xdp, 1e-2937_xdp, 1e-2936_xdp, &
        1e-2935_xdp, 1e-2934_xdp, 1e-2933_xdp, 1e-2932_xdp, 1e-2931_xdp, &
        1e-2930_xdp, 1e-2929_xdp, 1e-2928_xdp, 1e-2927_xdp, 1e-2926_xdp, &
        1e-2925_xdp, 1e-2924_xdp, 1e-2923_xdp, 1e-2922_xdp, 1e-2921_xdp, &
        1e-2920_xdp, 1e-2919_xdp, 1e-2918_xdp, 1e-2917_xdp, 1e-2916_xdp, &
        1e-2915_xdp, 1e-2914_xdp, 1e-2913_xdp, 1e-2912_xdp, 1e-2911_xdp, &
        1e-2910_xdp, 1e-2909_xdp, 1e-2908_xdp, 1e-2907_xdp, 1e-2906_xdp, &
        1e-2905_xdp, 1e-2904_xdp, 1e-2903_xdp, 1e-2902_xdp, 1e-2901_xdp, &
        1e-2900_xdp, 1e-2899_xdp, 1e-2898_xdp, 1e-2897_xdp, 1e-2896_xdp, &
        1e-2895_xdp, 1e-2894_xdp, 1e-2893_xdp, 1e-2892_xdp, 1e-2891_xdp, &
        1e-2890_xdp, 1e-2889_xdp, 1e-2888_xdp, 1e-2887_xdp, 1e-2886_xdp, &
        1e-2885_xdp, 1e-2884_xdp, 1e-2883_xdp, 1e-2882_xdp, 1e-2881_xdp, &
        1e-2880_xdp, 1e-2879_xdp, 1e-2878_xdp, 1e-2877_xdp, 1e-2876_xdp, &
        1e-2875_xdp, 1e-2874_xdp, 1e-2873_xdp, 1e-2872_xdp, 1e-2871_xdp, &
        1e-2870_xdp, 1e-2869_xdp, 1e-2868_xdp, 1e-2867_xdp, 1e-2866_xdp, &
        1e-2865_xdp, 1e-2864_xdp, 1e-2863_xdp, 1e-2862_xdp, 1e-2861_xdp, &
        1e-2860_xdp, 1e-2859_xdp, 1e-2858_xdp, 1e-2857_xdp, 1e-2856_xdp, &
        1e-2855_xdp, 1e-2854_xdp, 1e-2853_xdp, 1e-2852_xdp, 1e-2851_xdp, &
        1e-2850_xdp, 1e-2849_xdp, 1e-2848_xdp, 1e-2847_xdp, 1e-2846_xdp, &
        1e-2845_xdp, 1e-2844_xdp, 1e-2843_xdp, 1e-2842_xdp, 1e-2841_xdp, &
        1e-2840_xdp, 1e-2839_xdp, 1e-2838_xdp, 1e-2837_xdp, 1e-2836_xdp, &
        1e-2835_xdp, 1e-2834_xdp, 1e-2833_xdp, 1e-2832_xdp, 1e-2831_xdp, &
        1e-2830_xdp, 1e-2829_xdp, 1e-2828_xdp, 1e-2827_xdp, 1e-2826_xdp, &
        1e-2825_xdp, 1e-2824_xdp, 1e-2823_xdp, 1e-2822_xdp, 1e-2821_xdp, &
        1e-2820_xdp, 1e-2819_xdp, 1e-2818_xdp, 1e-2817_xdp, 1e-2816_xdp, &
        1e-2815_xdp, 1e-2814_xdp, 1e-2813_xdp, 1e-2812_xdp, 1e-2811_xdp, &
        1e-2810_xdp, 1e-2809_xdp, 1e-2808_xdp, 1e-2807_xdp, 1e-2806_xdp, &
        1e-2805_xdp, 1e-2804_xdp, 1e-2803_xdp, 1e-2802_xdp, 1e-2801_xdp, &
        1e-2800_xdp, 1e-2799_xdp, 1e-2798_xdp, 1e-2797_xdp, 1e-2796_xdp, &
        1e-2795_xdp, 1e-2794_xdp, 1e-2793_xdp, 1e-2792_xdp, 1e-2791_xdp, &
        1e-2790_xdp, 1e-2789_xdp, 1e-2788_xdp, 1e-2787_xdp, 1e-2786_xdp, &
        1e-2785_xdp, 1e-2784_xdp, 1e-2783_xdp, 1e-2782_xdp, 1e-2781_xdp, &
        1e-2780_xdp, 1e-2779_xdp, 1e-2778_xdp, 1e-2777_xdp, 1e-2776_xdp, &
        1e-2775_xdp, 1e-2774_xdp, 1e-2773_xdp, 1e-2772_xdp, 1e-2771_xdp, &
        1e-2770_xdp, 1e-2769_xdp, 1e-2768_xdp, 1e-2767_xdp, 1e-2766_xdp, &
        1e-2765_xdp, 1e-2764_xdp, 1e-2763_xdp, 1e-2762_xdp, 1e-2761_xdp, &
        1e-2760_xdp, 1e-2759_xdp, 1e-2758_xdp, 1e-2757_xdp, 1e-2756_xdp, &
        1e-2755_xdp, 1e-2754_xdp, 1e-2753_xdp, 1e-2752_xdp, 1e-2751_xdp, &
        1e-2750_xdp, 1e-2749_xdp, 1e-2748_xdp, 1e-2747_xdp, 1e-2746_xdp, &
        1e-2745_xdp, 1e-2744_xdp, 1e-2743_xdp, 1e-2742_xdp, 1e-2741_xdp, &
        1e-2740_xdp, 1e-2739_xdp, 1e-2738_xdp, 1e-2737_xdp, 1e-2736_xdp, &
        1e-2735_xdp, 1e-2734_xdp, 1e-2733_xdp, 1e-2732_xdp, 1e-2731_xdp, &
        1e-2730_xdp, 1e-2729_xdp, 1e-2728_xdp, 1e-2727_xdp, 1e-2726_xdp, &
        1e-2725_xdp, 1e-2724_xdp, 1e-2723_xdp, 1e-2722_xdp, 1e-2721_xdp, &
        1e-2720_xdp, 1e-2719_xdp, 1e-2718_xdp, 1e-2717_xdp, 1e-2716_xdp, &
        1e-2715_xdp, 1e-2714_xdp, 1e-2713_xdp, 1e-2712_xdp, 1e-2711_xdp, &
        1e-2710_xdp, 1e-2709_xdp, 1e-2708_xdp, 1e-2707_xdp, 1e-2706_xdp, &
        1e-2705_xdp, 1e-2704_xdp, 1e-2703_xdp, 1e-2702_xdp, 1e-2701_xdp, &
        1e-2700_xdp, 1e-2699_xdp, 1e-2698_xdp, 1e-2697_xdp, 1e-2696_xdp, &
        1e-2695_xdp, 1e-2694_xdp, 1e-2693_xdp, 1e-2692_xdp, 1e-2691_xdp, &
        1e-2690_xdp, 1e-2689_xdp, 1e-2688_xdp, 1e-2687_xdp, 1e-2686_xdp, &
        1e-2685_xdp, 1e-2684_xdp, 1e-2683_xdp, 1e-2682_xdp, 1e-2681_xdp, &
        1e-2680_xdp, 1e-2679_xdp, 1e-2678_xdp, 1e-2677_xdp, 1e-2676_xdp, &
        1e-2675_xdp, 1e-2674_xdp, 1e-2673_xdp, 1e-2672_xdp, 1e-2671_xdp, &
        1e-2670_xdp, 1e-2669_xdp, 1e-2668_xdp, 1e-2667_xdp, 1e-2666_xdp, &
        1e-2665_xdp, 1e-2664_xdp, 1e-2663_xdp, 1e-2662_xdp, 1e-2661_xdp, &
        1e-2660_xdp, 1e-2659_xdp, 1e-2658_xdp, 1e-2657_xdp, 1e-2656_xdp, &
        1e-2655_xdp, 1e-2654_xdp, 1e-2653_xdp, 1e-2652_xdp, 1e-2651_xdp, &
        1e-2650_xdp, 1e-2649_xdp, 1e-2648_xdp, 1e-2647_xdp, 1e-2646_xdp, &
        1e-2645_xdp, 1e-2644_xdp, 1e-2643_xdp, 1e-2642_xdp, 1e-2641_xdp, &
        1e-2640_xdp, 1e-2639_xdp, 1e-2638_xdp, 1e-2637_xdp, 1e-2636_xdp, &
        1e-2635_xdp, 1e-2634_xdp, 1e-2633_xdp, 1e-2632_xdp, 1e-2631_xdp, &
        1e-2630_xdp, 1e-2629_xdp, 1e-2628_xdp, 1e-2627_xdp, 1e-2626_xdp, &
        1e-2625_xdp, 1e-2624_xdp, 1e-2623_xdp, 1e-2622_xdp, 1e-2621_xdp, &
        1e-2620_xdp, 1e-2619_xdp, 1e-2618_xdp, 1e-2617_xdp, 1e-2616_xdp, &
        1e-2615_xdp, 1e-2614_xdp, 1e-2613_xdp, 1e-2612_xdp, 1e-2611_xdp, &
        1e-2610_xdp, 1e-2609_xdp, 1e-2608_xdp, 1e-2607_xdp, 1e-2606_xdp, &
        1e-2605_xdp, 1e-2604_xdp, 1e-2603_xdp, 1e-2602_xdp, 1e-2601_xdp, &
        1e-2600_xdp, 1e-2599_xdp, 1e-2598_xdp, 1e-2597_xdp, 1e-2596_xdp, &
        1e-2595_xdp, 1e-2594_xdp, 1e-2593_xdp, 1e-2592_xdp, 1e-2591_xdp, &
        1e-2590_xdp, 1e-2589_xdp, 1e-2588_xdp, 1e-2587_xdp, 1e-2586_xdp, &
        1e-2585_xdp, 1e-2584_xdp, 1e-2583_xdp, 1e-2582_xdp, 1e-2581_xdp, &
        1e-2580_xdp, 1e-2579_xdp, 1e-2578_xdp, 1e-2577_xdp, 1e-2576_xdp, &
        1e-2575_xdp, 1e-2574_xdp, 1e-2573_xdp, 1e-2572_xdp, 1e-2571_xdp, &
        1e-2570_xdp, 1e-2569_xdp, 1e-2568_xdp, 1e-2567_xdp, 1e-2566_xdp, &
        1e-2565_xdp, 1e-2564_xdp, 1e-2563_xdp, 1e-2562_xdp, 1e-2561_xdp, &
        1e-2560_xdp, 1e-2559_xdp, 1e-2558_xdp, 1e-2557_xdp, 1e-2556_xdp, &
        1e-2555_xdp, 1e-2554_xdp, 1e-2553_xdp, 1e-2552_xdp, 1e-2551_xdp, &
        1e-2550_xdp, 1e-2549_xdp, 1e-2548_xdp, 1e-2547_xdp, 1e-2546_xdp, &
        1e-2545_xdp, 1e-2544_xdp, 1e-2543_xdp, 1e-2542_xdp, 1e-2541_xdp, &
        1e-2540_xdp, 1e-2539_xdp, 1e-2538_xdp, 1e-2537_xdp, 1e-2536_xdp, &
        1e-2535_xdp, 1e-2534_xdp, 1e-2533_xdp, 1e-2532_xdp, 1e-2531_xdp, &
        1e-2530_xdp, 1e-2529_xdp, 1e-2528_xdp, 1e-2527_xdp, 1e-2526_xdp, &
        1e-2525_xdp, 1e-2524_xdp, 1e-2523_xdp, 1e-2522_xdp, 1e-2521_xdp, &
        1e-2520_xdp, 1e-2519_xdp, 1e-2518_xdp, 1e-2517_xdp, 1e-2516_xdp, &
        1e-2515_xdp, 1e-2514_xdp, 1e-2513_xdp, 1e-2512_xdp, 1e-2511_xdp, &
        1e-2510_xdp, 1e-2509_xdp, 1e-2508_xdp, 1e-2507_xdp, 1e-2506_xdp, &
        1e-2505_xdp, 1e-2504_xdp, 1e-2503_xdp, 1e-2502_xdp, 1e-2501_xdp, &
        1e-2500_xdp, 1e-2499_xdp, 1e-2498_xdp, 1e-2497_xdp, 1e-2496_xdp, &
        1e-2495_xdp, 1e-2494_xdp, 1e-2493_xdp, 1e-2492_xdp, 1e-2491_xdp, &
        1e-2490_xdp, 1e-2489_xdp, 1e-2488_xdp, 1e-2487_xdp, 1e-2486_xdp, &
        1e-2485_xdp, 1e-2484_xdp, 1e-2483_xdp, 1e-2482_xdp, 1e-2481_xdp, &
        1e-2480_xdp, 1e-2479_xdp, 1e-2478_xdp, 1e-2477_xdp, 1e-2476_xdp, &
        1e-2475_xdp, 1e-2474_xdp, 1e-2473_xdp, 1e-2472_xdp, 1e-2471_xdp, &
        1e-2470_xdp, 1e-2469_xdp, 1e-2468_xdp, 1e-2467_xdp, 1e-2466_xdp, &
        1e-2465_xdp, 1e-2464_xdp, 1e-2463_xdp, 1e-2462_xdp, 1e-2461_xdp, &
        1e-2460_xdp, 1e-2459_xdp, 1e-2458_xdp, 1e-2457_xdp, 1e-2456_xdp, &
        1e-2455_xdp, 1e-2454_xdp, 1e-2453_xdp, 1e-2452_xdp, 1e-2451_xdp, &
        1e-2450_xdp, 1e-2449_xdp, 1e-2448_xdp, 1e-2447_xdp, 1e-2446_xdp, &
        1e-2445_xdp, 1e-2444_xdp, 1e-2443_xdp, 1e-2442_xdp, 1e-2441_xdp, &
        1e-2440_xdp, 1e-2439_xdp, 1e-2438_xdp, 1e-2437_xdp, 1e-2436_xdp, &
        1e-2435_xdp, 1e-2434_xdp, 1e-2433_xdp, 1e-2432_xdp, 1e-2431_xdp, &
        1e-2430_xdp, 1e-2429_xdp, 1e-2428_xdp, 1e-2427_xdp, 1e-2426_xdp, &
        1e-2425_xdp, 1e-2424_xdp, 1e-2423_xdp, 1e-2422_xdp, 1e-2421_xdp, &
        1e-2420_xdp, 1e-2419_xdp, 1e-2418_xdp, 1e-2417_xdp, 1e-2416_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS3(*) = [ &
        1e-2415_xdp, 1e-2414_xdp, 1e-2413_xdp, 1e-2412_xdp, 1e-2411_xdp, &
        1e-2410_xdp, 1e-2409_xdp, 1e-2408_xdp, 1e-2407_xdp, 1e-2406_xdp, &
        1e-2405_xdp, 1e-2404_xdp, 1e-2403_xdp, 1e-2402_xdp, 1e-2401_xdp, &
        1e-2400_xdp, 1e-2399_xdp, 1e-2398_xdp, 1e-2397_xdp, 1e-2396_xdp, &
        1e-2395_xdp, 1e-2394_xdp, 1e-2393_xdp, 1e-2392_xdp, 1e-2391_xdp, &
        1e-2390_xdp, 1e-2389_xdp, 1e-2388_xdp, 1e-2387_xdp, 1e-2386_xdp, &
        1e-2385_xdp, 1e-2384_xdp, 1e-2383_xdp, 1e-2382_xdp, 1e-2381_xdp, &
        1e-2380_xdp, 1e-2379_xdp, 1e-2378_xdp, 1e-2377_xdp, 1e-2376_xdp, &
        1e-2375_xdp, 1e-2374_xdp, 1e-2373_xdp, 1e-2372_xdp, 1e-2371_xdp, &
        1e-2370_xdp, 1e-2369_xdp, 1e-2368_xdp, 1e-2367_xdp, 1e-2366_xdp, &
        1e-2365_xdp, 1e-2364_xdp, 1e-2363_xdp, 1e-2362_xdp, 1e-2361_xdp, &
        1e-2360_xdp, 1e-2359_xdp, 1e-2358_xdp, 1e-2357_xdp, 1e-2356_xdp, &
        1e-2355_xdp, 1e-2354_xdp, 1e-2353_xdp, 1e-2352_xdp, 1e-2351_xdp, &
        1e-2350_xdp, 1e-2349_xdp, 1e-2348_xdp, 1e-2347_xdp, 1e-2346_xdp, &
        1e-2345_xdp, 1e-2344_xdp, 1e-2343_xdp, 1e-2342_xdp, 1e-2341_xdp, &
        1e-2340_xdp, 1e-2339_xdp, 1e-2338_xdp, 1e-2337_xdp, 1e-2336_xdp, &
        1e-2335_xdp, 1e-2334_xdp, 1e-2333_xdp, 1e-2332_xdp, 1e-2331_xdp, &
        1e-2330_xdp, 1e-2329_xdp, 1e-2328_xdp, 1e-2327_xdp, 1e-2326_xdp, &
        1e-2325_xdp, 1e-2324_xdp, 1e-2323_xdp, 1e-2322_xdp, 1e-2321_xdp, &
        1e-2320_xdp, 1e-2319_xdp, 1e-2318_xdp, 1e-2317_xdp, 1e-2316_xdp, &
        1e-2315_xdp, 1e-2314_xdp, 1e-2313_xdp, 1e-2312_xdp, 1e-2311_xdp, &
        1e-2310_xdp, 1e-2309_xdp, 1e-2308_xdp, 1e-2307_xdp, 1e-2306_xdp, &
        1e-2305_xdp, 1e-2304_xdp, 1e-2303_xdp, 1e-2302_xdp, 1e-2301_xdp, &
        1e-2300_xdp, 1e-2299_xdp, 1e-2298_xdp, 1e-2297_xdp, 1e-2296_xdp, &
        1e-2295_xdp, 1e-2294_xdp, 1e-2293_xdp, 1e-2292_xdp, 1e-2291_xdp, &
        1e-2290_xdp, 1e-2289_xdp, 1e-2288_xdp, 1e-2287_xdp, 1e-2286_xdp, &
        1e-2285_xdp, 1e-2284_xdp, 1e-2283_xdp, 1e-2282_xdp, 1e-2281_xdp, &
        1e-2280_xdp, 1e-2279_xdp, 1e-2278_xdp, 1e-2277_xdp, 1e-2276_xdp, &
        1e-2275_xdp, 1e-2274_xdp, 1e-2273_xdp, 1e-2272_xdp, 1e-2271_xdp, &
        1e-2270_xdp, 1e-2269_xdp, 1e-2268_xdp, 1e-2267_xdp, 1e-2266_xdp, &
        1e-2265_xdp, 1e-2264_xdp, 1e-2263_xdp, 1e-2262_xdp, 1e-2261_xdp, &
        1e-2260_xdp, 1e-2259_xdp, 1e-2258_xdp, 1e-2257_xdp, 1e-2256_xdp, &
        1e-2255_xdp, 1e-2254_xdp, 1e-2253_xdp, 1e-2252_xdp, 1e-2251_xdp, &
        1e-2250_xdp, 1e-2249_xdp, 1e-2248_xdp, 1e-2247_xdp, 1e-2246_xdp, &
        1e-2245_xdp, 1e-2244_xdp, 1e-2243_xdp, 1e-2242_xdp, 1e-2241_xdp, &
        1e-2240_xdp, 1e-2239_xdp, 1e-2238_xdp, 1e-2237_xdp, 1e-2236_xdp, &
        1e-2235_xdp, 1e-2234_xdp, 1e-2233_xdp, 1e-2232_xdp, 1e-2231_xdp, &
        1e-2230_xdp, 1e-2229_xdp, 1e-2228_xdp, 1e-2227_xdp, 1e-2226_xdp, &
        1e-2225_xdp, 1e-2224_xdp, 1e-2223_xdp, 1e-2222_xdp, 1e-2221_xdp, &
        1e-2220_xdp, 1e-2219_xdp, 1e-2218_xdp, 1e-2217_xdp, 1e-2216_xdp, &
        1e-2215_xdp, 1e-2214_xdp, 1e-2213_xdp, 1e-2212_xdp, 1e-2211_xdp, &
        1e-2210_xdp, 1e-2209_xdp, 1e-2208_xdp, 1e-2207_xdp, 1e-2206_xdp, &
        1e-2205_xdp, 1e-2204_xdp, 1e-2203_xdp, 1e-2202_xdp, 1e-2201_xdp, &
        1e-2200_xdp, 1e-2199_xdp, 1e-2198_xdp, 1e-2197_xdp, 1e-2196_xdp, &
        1e-2195_xdp, 1e-2194_xdp, 1e-2193_xdp, 1e-2192_xdp, 1e-2191_xdp, &
        1e-2190_xdp, 1e-2189_xdp, 1e-2188_xdp, 1e-2187_xdp, 1e-2186_xdp, &
        1e-2185_xdp, 1e-2184_xdp, 1e-2183_xdp, 1e-2182_xdp, 1e-2181_xdp, &
        1e-2180_xdp, 1e-2179_xdp, 1e-2178_xdp, 1e-2177_xdp, 1e-2176_xdp, &
        1e-2175_xdp, 1e-2174_xdp, 1e-2173_xdp, 1e-2172_xdp, 1e-2171_xdp, &
        1e-2170_xdp, 1e-2169_xdp, 1e-2168_xdp, 1e-2167_xdp, 1e-2166_xdp, &
        1e-2165_xdp, 1e-2164_xdp, 1e-2163_xdp, 1e-2162_xdp, 1e-2161_xdp, &
        1e-2160_xdp, 1e-2159_xdp, 1e-2158_xdp, 1e-2157_xdp, 1e-2156_xdp, &
        1e-2155_xdp, 1e-2154_xdp, 1e-2153_xdp, 1e-2152_xdp, 1e-2151_xdp, &
        1e-2150_xdp, 1e-2149_xdp, 1e-2148_xdp, 1e-2147_xdp, 1e-2146_xdp, &
        1e-2145_xdp, 1e-2144_xdp, 1e-2143_xdp, 1e-2142_xdp, 1e-2141_xdp, &
        1e-2140_xdp, 1e-2139_xdp, 1e-2138_xdp, 1e-2137_xdp, 1e-2136_xdp, &
        1e-2135_xdp, 1e-2134_xdp, 1e-2133_xdp, 1e-2132_xdp, 1e-2131_xdp, &
        1e-2130_xdp, 1e-2129_xdp, 1e-2128_xdp, 1e-2127_xdp, 1e-2126_xdp, &
        1e-2125_xdp, 1e-2124_xdp, 1e-2123_xdp, 1e-2122_xdp, 1e-2121_xdp, &
        1e-2120_xdp, 1e-2119_xdp, 1e-2118_xdp, 1e-2117_xdp, 1e-2116_xdp, &
        1e-2115_xdp, 1e-2114_xdp, 1e-2113_xdp, 1e-2112_xdp, 1e-2111_xdp, &
        1e-2110_xdp, 1e-2109_xdp, 1e-2108_xdp, 1e-2107_xdp, 1e-2106_xdp, &
        1e-2105_xdp, 1e-2104_xdp, 1e-2103_xdp, 1e-2102_xdp, 1e-2101_xdp, &
        1e-2100_xdp, 1e-2099_xdp, 1e-2098_xdp, 1e-2097_xdp, 1e-2096_xdp, &
        1e-2095_xdp, 1e-2094_xdp, 1e-2093_xdp, 1e-2092_xdp, 1e-2091_xdp, &
        1e-2090_xdp, 1e-2089_xdp, 1e-2088_xdp, 1e-2087_xdp, 1e-2086_xdp, &
        1e-2085_xdp, 1e-2084_xdp, 1e-2083_xdp, 1e-2082_xdp, 1e-2081_xdp, &
        1e-2080_xdp, 1e-2079_xdp, 1e-2078_xdp, 1e-2077_xdp, 1e-2076_xdp, &
        1e-2075_xdp, 1e-2074_xdp, 1e-2073_xdp, 1e-2072_xdp, 1e-2071_xdp, &
        1e-2070_xdp, 1e-2069_xdp, 1e-2068_xdp, 1e-2067_xdp, 1e-2066_xdp, &
        1e-2065_xdp, 1e-2064_xdp, 1e-2063_xdp, 1e-2062_xdp, 1e-2061_xdp, &
        1e-2060_xdp, 1e-2059_xdp, 1e-2058_xdp, 1e-2057_xdp, 1e-2056_xdp, &
        1e-2055_xdp, 1e-2054_xdp, 1e-2053_xdp, 1e-2052_xdp, 1e-2051_xdp, &
        1e-2050_xdp, 1e-2049_xdp, 1e-2048_xdp, 1e-2047_xdp, 1e-2046_xdp, &
        1e-2045_xdp, 1e-2044_xdp, 1e-2043_xdp, 1e-2042_xdp, 1e-2041_xdp, &
        1e-2040_xdp, 1e-2039_xdp, 1e-2038_xdp, 1e-2037_xdp, 1e-2036_xdp, &
        1e-2035_xdp, 1e-2034_xdp, 1e-2033_xdp, 1e-2032_xdp, 1e-2031_xdp, &
        1e-2030_xdp, 1e-2029_xdp, 1e-2028_xdp, 1e-2027_xdp, 1e-2026_xdp, &
        1e-2025_xdp, 1e-2024_xdp, 1e-2023_xdp, 1e-2022_xdp, 1e-2021_xdp, &
        1e-2020_xdp, 1e-2019_xdp, 1e-2018_xdp, 1e-2017_xdp, 1e-2016_xdp, &
        1e-2015_xdp, 1e-2014_xdp, 1e-2013_xdp, 1e-2012_xdp, 1e-2011_xdp, &
        1e-2010_xdp, 1e-2009_xdp, 1e-2008_xdp, 1e-2007_xdp, 1e-2006_xdp, &
        1e-2005_xdp, 1e-2004_xdp, 1e-2003_xdp, 1e-2002_xdp, 1e-2001_xdp, &
        1e-2000_xdp, 1e-1999_xdp, 1e-1998_xdp, 1e-1997_xdp, 1e-1996_xdp, &
        1e-1995_xdp, 1e-1994_xdp, 1e-1993_xdp, 1e-1992_xdp, 1e-1991_xdp, &
        1e-1990_xdp, 1e-1989_xdp, 1e-1988_xdp, 1e-1987_xdp, 1e-1986_xdp, &
        1e-1985_xdp, 1e-1984_xdp, 1e-1983_xdp, 1e-1982_xdp, 1e-1981_xdp, &
        1e-1980_xdp, 1e-1979_xdp, 1e-1978_xdp, 1e-1977_xdp, 1e-1976_xdp, &
        1e-1975_xdp, 1e-1974_xdp, 1e-1973_xdp, 1e-1972_xdp, 1e-1971_xdp, &
        1e-1970_xdp, 1e-1969_xdp, 1e-1968_xdp, 1e-1967_xdp, 1e-1966_xdp, &
        1e-1965_xdp, 1e-1964_xdp, 1e-1963_xdp, 1e-1962_xdp, 1e-1961_xdp, &
        1e-1960_xdp, 1e-1959_xdp, 1e-1958_xdp, 1e-1957_xdp, 1e-1956_xdp, &
        1e-1955_xdp, 1e-1954_xdp, 1e-1953_xdp, 1e-1952_xdp, 1e-1951_xdp, &
        1e-1950_xdp, 1e-1949_xdp, 1e-1948_xdp, 1e-1947_xdp, 1e-1946_xdp, &
        1e-1945_xdp, 1e-1944_xdp, 1e-1943_xdp, 1e-1942_xdp, 1e-1941_xdp, &
        1e-1940_xdp, 1e-1939_xdp, 1e-1938_xdp, 1e-1937_xdp, 1e-1936_xdp, &
        1e-1935_xdp, 1e-1934_xdp, 1e-1933_xdp, 1e-1932_xdp, 1e-1931_xdp, &
        1e-1930_xdp, 1e-1929_xdp, 1e-1928_xdp, 1e-1927_xdp, 1e-1926_xdp, &
        1e-1925_xdp, 1e-1924_xdp, 1e-1923_xdp, 1e-1922_xdp, 1e-1921_xdp, &
        1e-1920_xdp, 1e-1919_xdp, 1e-1918_xdp, 1e-1917_xdp, 1e-1916_xdp, &
        1e-1915_xdp, 1e-1914_xdp, 1e-1913_xdp, 1e-1912_xdp, 1e-1911_xdp, &
        1e-1910_xdp, 1e-1909_xdp, 1e-1908_xdp, 1e-1907_xdp, 1e-1906_xdp, &
        1e-1905_xdp, 1e-1904_xdp, 1e-1903_xdp, 1e-1902_xdp, 1e-1901_xdp, &
        1e-1900_xdp, 1e-1899_xdp, 1e-1898_xdp, 1e-1897_xdp, 1e-1896_xdp, &
        1e-1895_xdp, 1e-1894_xdp, 1e-1893_xdp, 1e-1892_xdp, 1e-1891_xdp, &
        1e-1890_xdp, 1e-1889_xdp, 1e-1888_xdp, 1e-1887_xdp, 1e-1886_xdp, &
        1e-1885_xdp, 1e-1884_xdp, 1e-1883_xdp, 1e-1882_xdp, 1e-1881_xdp, &
        1e-1880_xdp, 1e-1879_xdp, 1e-1878_xdp, 1e-1877_xdp, 1e-1876_xdp, &
        1e-1875_xdp, 1e-1874_xdp, 1e-1873_xdp, 1e-1872_xdp, 1e-1871_xdp, &
        1e-1870_xdp, 1e-1869_xdp, 1e-1868_xdp, 1e-1867_xdp, 1e-1866_xdp, &
        1e-1865_xdp, 1e-1864_xdp, 1e-1863_xdp, 1e-1862_xdp, 1e-1861_xdp, &
        1e-1860_xdp, 1e-1859_xdp, 1e-1858_xdp, 1e-1857_xdp, 1e-1856_xdp, &
        1e-1855_xdp, 1e-1854_xdp, 1e-1853_xdp, 1e-1852_xdp, 1e-1851_xdp, &
        1e-1850_xdp, 1e-1849_xdp, 1e-1848_xdp, 1e-1847_xdp, 1e-1846_xdp, &
        1e-1845_xdp, 1e-1844_xdp, 1e-1843_xdp, 1e-1842_xdp, 1e-1841_xdp, &
        1e-1840_xdp, 1e-1839_xdp, 1e-1838_xdp, 1e-1837_xdp, 1e-1836_xdp, &
        1e-1835_xdp, 1e-1834_xdp, 1e-1833_xdp, 1e-1832_xdp, 1e-1831_xdp, &
        1e-1830_xdp, 1e-1829_xdp, 1e-1828_xdp, 1e-1827_xdp, 1e-1826_xdp, &
        1e-1825_xdp, 1e-1824_xdp, 1e-1823_xdp, 1e-1822_xdp, 1e-1821_xdp, &
        1e-1820_xdp, 1e-1819_xdp, 1e-1818_xdp, 1e-1817_xdp, 1e-1816_xdp, &
        1e-1815_xdp, 1e-1814_xdp, 1e-1813_xdp, 1e-1812_xdp, 1e-1811_xdp, &
        1e-1810_xdp, 1e-1809_xdp, 1e-1808_xdp, 1e-1807_xdp, 1e-1806_xdp, &
        1e-1805_xdp, 1e-1804_xdp, 1e-1803_xdp, 1e-1802_xdp, 1e-1801_xdp, &
        1e-1800_xdp, 1e-1799_xdp, 1e-1798_xdp, 1e-1797_xdp, 1e-1796_xdp, &
        1e-1795_xdp, 1e-1794_xdp, 1e-1793_xdp, 1e-1792_xdp, 1e-1791_xdp, &
        1e-1790_xdp, 1e-1789_xdp, 1e-1788_xdp, 1e-1787_xdp, 1e-1786_xdp, &
        1e-1785_xdp, 1e-1784_xdp, 1e-1783_xdp, 1e-1782_xdp, 1e-1781_xdp, &
        1e-1780_xdp, 1e-1779_xdp, 1e-1778_xdp, 1e-1777_xdp, 1e-1776_xdp, &
        1e-1775_xdp, 1e-1774_xdp, 1e-1773_xdp, 1e-1772_xdp, 1e-1771_xdp, &
        1e-1770_xdp, 1e-1769_xdp, 1e-1768_xdp, 1e-1767_xdp, 1e-1766_xdp, &
        1e-1765_xdp, 1e-1764_xdp, 1e-1763_xdp, 1e-1762_xdp, 1e-1761_xdp, &
        1e-1760_xdp, 1e-1759_xdp, 1e-1758_xdp, 1e-1757_xdp, 1e-1756_xdp, &
        1e-1755_xdp, 1e-1754_xdp, 1e-1753_xdp, 1e-1752_xdp, 1e-1751_xdp, &
        1e-1750_xdp, 1e-1749_xdp, 1e-1748_xdp, 1e-1747_xdp, 1e-1746_xdp, &
        1e-1745_xdp, 1e-1744_xdp, 1e-1743_xdp, 1e-1742_xdp, 1e-1741_xdp, &
        1e-1740_xdp, 1e-1739_xdp, 1e-1738_xdp, 1e-1737_xdp, 1e-1736_xdp, &
        1e-1735_xdp, 1e-1734_xdp, 1e-1733_xdp, 1e-1732_xdp, 1e-1731_xdp, &
        1e-1730_xdp, 1e-1729_xdp, 1e-1728_xdp, 1e-1727_xdp, 1e-1726_xdp, &
        1e-1725_xdp, 1e-1724_xdp, 1e-1723_xdp, 1e-1722_xdp, 1e-1721_xdp, &
        1e-1720_xdp, 1e-1719_xdp, 1e-1718_xdp, 1e-1717_xdp, 1e-1716_xdp, &
        1e-1715_xdp, 1e-1714_xdp, 1e-1713_xdp, 1e-1712_xdp, 1e-1711_xdp, &
        1e-1710_xdp, 1e-1709_xdp, 1e-1708_xdp, 1e-1707_xdp, 1e-1706_xdp, &
        1e-1705_xdp, 1e-1704_xdp, 1e-1703_xdp, 1e-1702_xdp, 1e-1701_xdp, &
        1e-1700_xdp, 1e-1699_xdp, 1e-1698_xdp, 1e-1697_xdp, 1e-1696_xdp, &
        1e-1695_xdp, 1e-1694_xdp, 1e-1693_xdp, 1e-1692_xdp, 1e-1691_xdp, &
        1e-1690_xdp, 1e-1689_xdp, 1e-1688_xdp, 1e-1687_xdp, 1e-1686_xdp, &
        1e-1685_xdp, 1e-1684_xdp, 1e-1683_xdp, 1e-1682_xdp, 1e-1681_xdp, &
        1e-1680_xdp, 1e-1679_xdp, 1e-1678_xdp, 1e-1677_xdp, 1e-1676_xdp, &
        1e-1675_xdp, 1e-1674_xdp, 1e-1673_xdp, 1e-1672_xdp, 1e-1671_xdp, &
        1e-1670_xdp, 1e-1669_xdp, 1e-1668_xdp, 1e-1667_xdp, 1e-1666_xdp, &
        1e-1665_xdp, 1e-1664_xdp, 1e-1663_xdp, 1e-1662_xdp, 1e-1661_xdp, &
        1e-1660_xdp, 1e-1659_xdp, 1e-1658_xdp, 1e-1657_xdp, 1e-1656_xdp, &
        1e-1655_xdp, 1e-1654_xdp, 1e-1653_xdp, 1e-1652_xdp, 1e-1651_xdp, &
        1e-1650_xdp, 1e-1649_xdp, 1e-1648_xdp, 1e-1647_xdp, 1e-1646_xdp, &
        1e-1645_xdp, 1e-1644_xdp, 1e-1643_xdp, 1e-1642_xdp, 1e-1641_xdp, &
        1e-1640_xdp, 1e-1639_xdp, 1e-1638_xdp, 1e-1637_xdp, 1e-1636_xdp, &
        1e-1635_xdp, 1e-1634_xdp, 1e-1633_xdp, 1e-1632_xdp, 1e-1631_xdp, &
        1e-1630_xdp, 1e-1629_xdp, 1e-1628_xdp, 1e-1627_xdp, 1e-1626_xdp, &
        1e-1625_xdp, 1e-1624_xdp, 1e-1623_xdp, 1e-1622_xdp, 1e-1621_xdp, &
        1e-1620_xdp, 1e-1619_xdp, 1e-1618_xdp, 1e-1617_xdp, 1e-1616_xdp, &
        1e-1615_xdp, 1e-1614_xdp, 1e-1613_xdp, 1e-1612_xdp, 1e-1611_xdp, &
        1e-1610_xdp, 1e-1609_xdp, 1e-1608_xdp, 1e-1607_xdp, 1e-1606_xdp, &
        1e-1605_xdp, 1e-1604_xdp, 1e-1603_xdp, 1e-1602_xdp, 1e-1601_xdp, &
        1e-1600_xdp, 1e-1599_xdp, 1e-1598_xdp, 1e-1597_xdp, 1e-1596_xdp, &
        1e-1595_xdp, 1e-1594_xdp, 1e-1593_xdp, 1e-1592_xdp, 1e-1591_xdp, &
        1e-1590_xdp, 1e-1589_xdp, 1e-1588_xdp, 1e-1587_xdp, 1e-1586_xdp, &
        1e-1585_xdp, 1e-1584_xdp, 1e-1583_xdp, 1e-1582_xdp, 1e-1581_xdp, &
        1e-1580_xdp, 1e-1579_xdp, 1e-1578_xdp, 1e-1577_xdp, 1e-1576_xdp, &
        1e-1575_xdp, 1e-1574_xdp, 1e-1573_xdp, 1e-1572_xdp, 1e-1571_xdp, &
        1e-1570_xdp, 1e-1569_xdp, 1e-1568_xdp, 1e-1567_xdp, 1e-1566_xdp, &
        1e-1565_xdp, 1e-1564_xdp, 1e-1563_xdp, 1e-1562_xdp, 1e-1561_xdp, &
        1e-1560_xdp, 1e-1559_xdp, 1e-1558_xdp, 1e-1557_xdp, 1e-1556_xdp, &
        1e-1555_xdp, 1e-1554_xdp, 1e-1553_xdp, 1e-1552_xdp, 1e-1551_xdp, &
        1e-1550_xdp, 1e-1549_xdp, 1e-1548_xdp, 1e-1547_xdp, 1e-1546_xdp, &
        1e-1545_xdp, 1e-1544_xdp, 1e-1543_xdp, 1e-1542_xdp, 1e-1541_xdp, &
        1e-1540_xdp, 1e-1539_xdp, 1e-1538_xdp, 1e-1537_xdp, 1e-1536_xdp, &
        1e-1535_xdp, 1e-1534_xdp, 1e-1533_xdp, 1e-1532_xdp, 1e-1531_xdp, &
        1e-1530_xdp, 1e-1529_xdp, 1e-1528_xdp, 1e-1527_xdp, 1e-1526_xdp, &
        1e-1525_xdp, 1e-1524_xdp, 1e-1523_xdp, 1e-1522_xdp, 1e-1521_xdp, &
        1e-1520_xdp, 1e-1519_xdp, 1e-1518_xdp, 1e-1517_xdp, 1e-1516_xdp, &
        1e-1515_xdp, 1e-1514_xdp, 1e-1513_xdp, 1e-1512_xdp, 1e-1511_xdp, &
        1e-1510_xdp, 1e-1509_xdp, 1e-1508_xdp, 1e-1507_xdp, 1e-1506_xdp, &
        1e-1505_xdp, 1e-1504_xdp, 1e-1503_xdp, 1e-1502_xdp, 1e-1501_xdp, &
        1e-1500_xdp, 1e-1499_xdp, 1e-1498_xdp, 1e-1497_xdp, 1e-1496_xdp, &
        1e-1495_xdp, 1e-1494_xdp, 1e-1493_xdp, 1e-1492_xdp, 1e-1491_xdp, &
        1e-1490_xdp, 1e-1489_xdp, 1e-1488_xdp, 1e-1487_xdp, 1e-1486_xdp, &
        1e-1485_xdp, 1e-1484_xdp, 1e-1483_xdp, 1e-1482_xdp, 1e-1481_xdp, &
        1e-1480_xdp, 1e-1479_xdp, 1e-1478_xdp, 1e-1477_xdp, 1e-1476_xdp, &
        1e-1475_xdp, 1e-1474_xdp, 1e-1473_xdp, 1e-1472_xdp, 1e-1471_xdp, &
        1e-1470_xdp, 1e-1469_xdp, 1e-1468_xdp, 1e-1467_xdp, 1e-1466_xdp, &
        1e-1465_xdp, 1e-1464_xdp, 1e-1463_xdp, 1e-1462_xdp, 1e-1461_xdp, &
        1e-1460_xdp, 1e-1459_xdp, 1e-1458_xdp, 1e-1457_xdp, 1e-1456_xdp, &
        1e-1455_xdp, 1e-1454_xdp, 1e-1453_xdp, 1e-1452_xdp, 1e-1451_xdp, &
        1e-1450_xdp, 1e-1449_xdp, 1e-1448_xdp, 1e-1447_xdp, 1e-1446_xdp, &
        1e-1445_xdp, 1e-1444_xdp, 1e-1443_xdp, 1e-1442_xdp, 1e-1441_xdp, &
        1e-1440_xdp, 1e-1439_xdp, 1e-1438_xdp, 1e-1437_xdp, 1e-1436_xdp, &
        1e-1435_xdp, 1e-1434_xdp, 1e-1433_xdp, 1e-1432_xdp, 1e-1431_xdp, &
        1e-1430_xdp, 1e-1429_xdp, 1e-1428_xdp, 1e-1427_xdp, 1e-1426_xdp, &
        1e-1425_xdp, 1e-1424_xdp, 1e-1423_xdp, 1e-1422_xdp, 1e-1421_xdp, &
        1e-1420_xdp, 1e-1419_xdp, 1e-1418_xdp, 1e-1417_xdp, 1e-1416_xdp, &
        1e-1415_xdp, 1e-1414_xdp, 1e-1413_xdp, 1e-1412_xdp, 1e-1411_xdp, &
        1e-1410_xdp, 1e-1409_xdp, 1e-1408_xdp, 1e-1407_xdp, 1e-1406_xdp, &
        1e-1405_xdp, 1e-1404_xdp, 1e-1403_xdp, 1e-1402_xdp, 1e-1401_xdp, &
        1e-1400_xdp, 1e-1399_xdp, 1e-1398_xdp, 1e-1397_xdp, 1e-1396_xdp, &
        1e-1395_xdp, 1e-1394_xdp, 1e-1393_xdp, 1e-1392_xdp, 1e-1391_xdp, &
        1e-1390_xdp, 1e-1389_xdp, 1e-1388_xdp, 1e-1387_xdp, 1e-1386_xdp, &
        1e-1385_xdp, 1e-1384_xdp, 1e-1383_xdp, 1e-1382_xdp, 1e-1381_xdp, &
        1e-1380_xdp, 1e-1379_xdp, 1e-1378_xdp, 1e-1377_xdp, 1e-1376_xdp, &
        1e-1375_xdp, 1e-1374_xdp, 1e-1373_xdp, 1e-1372_xdp, 1e-1371_xdp, &
        1e-1370_xdp, 1e-1369_xdp, 1e-1368_xdp, 1e-1367_xdp, 1e-1366_xdp, &
        1e-1365_xdp, 1e-1364_xdp, 1e-1363_xdp, 1e-1362_xdp, 1e-1361_xdp, &
        1e-1360_xdp, 1e-1359_xdp, 1e-1358_xdp, 1e-1357_xdp, 1e-1356_xdp, &
        1e-1355_xdp, 1e-1354_xdp, 1e-1353_xdp, 1e-1352_xdp, 1e-1351_xdp, &
        1e-1350_xdp, 1e-1349_xdp, 1e-1348_xdp, 1e-1347_xdp, 1e-1346_xdp, &
        1e-1345_xdp, 1e-1344_xdp, 1e-1343_xdp, 1e-1342_xdp, 1e-1341_xdp, &
        1e-1340_xdp, 1e-1339_xdp, 1e-1338_xdp, 1e-1337_xdp, 1e-1336_xdp, &
        1e-1335_xdp, 1e-1334_xdp, 1e-1333_xdp, 1e-1332_xdp, 1e-1331_xdp, &
        1e-1330_xdp, 1e-1329_xdp, 1e-1328_xdp, 1e-1327_xdp, 1e-1326_xdp, &
        1e-1325_xdp, 1e-1324_xdp, 1e-1323_xdp, 1e-1322_xdp, 1e-1321_xdp, &
        1e-1320_xdp, 1e-1319_xdp, 1e-1318_xdp, 1e-1317_xdp, 1e-1316_xdp, &
        1e-1315_xdp, 1e-1314_xdp, 1e-1313_xdp, 1e-1312_xdp, 1e-1311_xdp, &
        1e-1310_xdp, 1e-1309_xdp, 1e-1308_xdp, 1e-1307_xdp, 1e-1306_xdp, &
        1e-1305_xdp, 1e-1304_xdp, 1e-1303_xdp, 1e-1302_xdp, 1e-1301_xdp, &
        1e-1300_xdp, 1e-1299_xdp, 1e-1298_xdp, 1e-1297_xdp, 1e-1296_xdp, &
        1e-1295_xdp, 1e-1294_xdp, 1e-1293_xdp, 1e-1292_xdp, 1e-1291_xdp, &
        1e-1290_xdp, 1e-1289_xdp, 1e-1288_xdp, 1e-1287_xdp, 1e-1286_xdp, &
        1e-1285_xdp, 1e-1284_xdp, 1e-1283_xdp, 1e-1282_xdp, 1e-1281_xdp, &
        1e-1280_xdp, 1e-1279_xdp, 1e-1278_xdp, 1e-1277_xdp, 1e-1276_xdp, &
        1e-1275_xdp, 1e-1274_xdp, 1e-1273_xdp, 1e-1272_xdp, 1e-1271_xdp, &
        1e-1270_xdp, 1e-1269_xdp, 1e-1268_xdp, 1e-1267_xdp, 1e-1266_xdp, &
        1e-1265_xdp, 1e-1264_xdp, 1e-1263_xdp, 1e-1262_xdp, 1e-1261_xdp, &
        1e-1260_xdp, 1e-1259_xdp, 1e-1258_xdp, 1e-1257_xdp, 1e-1256_xdp, &
        1e-1255_xdp, 1e-1254_xdp, 1e-1253_xdp, 1e-1252_xdp, 1e-1251_xdp, &
        1e-1250_xdp, 1e-1249_xdp, 1e-1248_xdp, 1e-1247_xdp, 1e-1246_xdp, &
        1e-1245_xdp, 1e-1244_xdp, 1e-1243_xdp, 1e-1242_xdp, 1e-1241_xdp, &
        1e-1240_xdp, 1e-1239_xdp, 1e-1238_xdp, 1e-1237_xdp, 1e-1236_xdp, &
        1e-1235_xdp, 1e-1234_xdp, 1e-1233_xdp, 1e-1232_xdp, 1e-1231_xdp, &
        1e-1230_xdp, 1e-1229_xdp, 1e-1228_xdp, 1e-1227_xdp, 1e-1226_xdp, &
        1e-1225_xdp, 1e-1224_xdp, 1e-1223_xdp, 1e-1222_xdp, 1e-1221_xdp, &
        1e-1220_xdp, 1e-1219_xdp, 1e-1218_xdp, 1e-1217_xdp, 1e-1216_xdp, &
        1e-1215_xdp, 1e-1214_xdp, 1e-1213_xdp, 1e-1212_xdp, 1e-1211_xdp, &
        1e-1210_xdp, 1e-1209_xdp, 1e-1208_xdp, 1e-1207_xdp, 1e-1206_xdp, &
        1e-1205_xdp, 1e-1204_xdp, 1e-1203_xdp, 1e-1202_xdp, 1e-1201_xdp, &
        1e-1200_xdp, 1e-1199_xdp, 1e-1198_xdp, 1e-1197_xdp, 1e-1196_xdp, &
        1e-1195_xdp, 1e-1194_xdp, 1e-1193_xdp, 1e-1192_xdp, 1e-1191_xdp, &
        1e-1190_xdp, 1e-1189_xdp, 1e-1188_xdp, 1e-1187_xdp, 1e-1186_xdp, &
        1e-1185_xdp, 1e-1184_xdp, 1e-1183_xdp, 1e-1182_xdp, 1e-1181_xdp, &
        1e-1180_xdp, 1e-1179_xdp, 1e-1178_xdp, 1e-1177_xdp, 1e-1176_xdp, &
        1e-1175_xdp, 1e-1174_xdp, 1e-1173_xdp, 1e-1172_xdp, 1e-1171_xdp, &
        1e-1170_xdp, 1e-1169_xdp, 1e-1168_xdp, 1e-1167_xdp, 1e-1166_xdp, &
        1e-1165_xdp, 1e-1164_xdp, 1e-1163_xdp, 1e-1162_xdp, 1e-1161_xdp, &
        1e-1160_xdp, 1e-1159_xdp, 1e-1158_xdp, 1e-1157_xdp, 1e-1156_xdp, &
        1e-1155_xdp, 1e-1154_xdp, 1e-1153_xdp, 1e-1152_xdp, 1e-1151_xdp, &
        1e-1150_xdp, 1e-1149_xdp, 1e-1148_xdp, 1e-1147_xdp, 1e-1146_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS4(*) = [ &
        1e-1145_xdp, 1e-1144_xdp, 1e-1143_xdp, 1e-1142_xdp, 1e-1141_xdp, &
        1e-1140_xdp, 1e-1139_xdp, 1e-1138_xdp, 1e-1137_xdp, 1e-1136_xdp, &
        1e-1135_xdp, 1e-1134_xdp, 1e-1133_xdp, 1e-1132_xdp, 1e-1131_xdp, &
        1e-1130_xdp, 1e-1129_xdp, 1e-1128_xdp, 1e-1127_xdp, 1e-1126_xdp, &
        1e-1125_xdp, 1e-1124_xdp, 1e-1123_xdp, 1e-1122_xdp, 1e-1121_xdp, &
        1e-1120_xdp, 1e-1119_xdp, 1e-1118_xdp, 1e-1117_xdp, 1e-1116_xdp, &
        1e-1115_xdp, 1e-1114_xdp, 1e-1113_xdp, 1e-1112_xdp, 1e-1111_xdp, &
        1e-1110_xdp, 1e-1109_xdp, 1e-1108_xdp, 1e-1107_xdp, 1e-1106_xdp, &
        1e-1105_xdp, 1e-1104_xdp, 1e-1103_xdp, 1e-1102_xdp, 1e-1101_xdp, &
        1e-1100_xdp, 1e-1099_xdp, 1e-1098_xdp, 1e-1097_xdp, 1e-1096_xdp, &
        1e-1095_xdp, 1e-1094_xdp, 1e-1093_xdp, 1e-1092_xdp, 1e-1091_xdp, &
        1e-1090_xdp, 1e-1089_xdp, 1e-1088_xdp, 1e-1087_xdp, 1e-1086_xdp, &
        1e-1085_xdp, 1e-1084_xdp, 1e-1083_xdp, 1e-1082_xdp, 1e-1081_xdp, &
        1e-1080_xdp, 1e-1079_xdp, 1e-1078_xdp, 1e-1077_xdp, 1e-1076_xdp, &
        1e-1075_xdp, 1e-1074_xdp, 1e-1073_xdp, 1e-1072_xdp, 1e-1071_xdp, &
        1e-1070_xdp, 1e-1069_xdp, 1e-1068_xdp, 1e-1067_xdp, 1e-1066_xdp, &
        1e-1065_xdp, 1e-1064_xdp, 1e-1063_xdp, 1e-1062_xdp, 1e-1061_xdp, &
        1e-1060_xdp, 1e-1059_xdp, 1e-1058_xdp, 1e-1057_xdp, 1e-1056_xdp, &
        1e-1055_xdp, 1e-1054_xdp, 1e-1053_xdp, 1e-1052_xdp, 1e-1051_xdp, &
        1e-1050_xdp, 1e-1049_xdp, 1e-1048_xdp, 1e-1047_xdp, 1e-1046_xdp, &
        1e-1045_xdp, 1e-1044_xdp, 1e-1043_xdp, 1e-1042_xdp, 1e-1041_xdp, &
        1e-1040_xdp, 1e-1039_xdp, 1e-1038_xdp, 1e-1037_xdp, 1e-1036_xdp, &
        1e-1035_xdp, 1e-1034_xdp, 1e-1033_xdp, 1e-1032_xdp, 1e-1031_xdp, &
        1e-1030_xdp, 1e-1029_xdp, 1e-1028_xdp, 1e-1027_xdp, 1e-1026_xdp, &
        1e-1025_xdp, 1e-1024_xdp, 1e-1023_xdp, 1e-1022_xdp, 1e-1021_xdp, &
        1e-1020_xdp, 1e-1019_xdp, 1e-1018_xdp, 1e-1017_xdp, 1e-1016_xdp, &
        1e-1015_xdp, 1e-1014_xdp, 1e-1013_xdp, 1e-1012_xdp, 1e-1011_xdp, &
        1e-1010_xdp, 1e-1009_xdp, 1e-1008_xdp, 1e-1007_xdp, 1e-1006_xdp, &
        1e-1005_xdp, 1e-1004_xdp, 1e-1003_xdp, 1e-1002_xdp, 1e-1001_xdp, &
        1e-1000_xdp, 1e-999_xdp, 1e-998_xdp, 1e-997_xdp, 1e-996_xdp, &
        1e-995_xdp, 1e-994_xdp, 1e-993_xdp, 1e-992_xdp, 1e-991_xdp, &
        1e-990_xdp, 1e-989_xdp, 1e-988_xdp, 1e-987_xdp, 1e-986_xdp, &
        1e-985_xdp, 1e-984_xdp, 1e-983_xdp, 1e-982_xdp, 1e-981_xdp, &
        1e-980_xdp, 1e-979_xdp, 1e-978_xdp, 1e-977_xdp, 1e-976_xdp, &
        1e-975_xdp, 1e-974_xdp, 1e-973_xdp, 1e-972_xdp, 1e-971_xdp, &
        1e-970_xdp, 1e-969_xdp, 1e-968_xdp, 1e-967_xdp, 1e-966_xdp, &
        1e-965_xdp, 1e-964_xdp, 1e-963_xdp, 1e-962_xdp, 1e-961_xdp, &
        1e-960_xdp, 1e-959_xdp, 1e-958_xdp, 1e-957_xdp, 1e-956_xdp, &
        1e-955_xdp, 1e-954_xdp, 1e-953_xdp, 1e-952_xdp, 1e-951_xdp, &
        1e-950_xdp, 1e-949_xdp, 1e-948_xdp, 1e-947_xdp, 1e-946_xdp, &
        1e-945_xdp, 1e-944_xdp, 1e-943_xdp, 1e-942_xdp, 1e-941_xdp, &
        1e-940_xdp, 1e-939_xdp, 1e-938_xdp, 1e-937_xdp, 1e-936_xdp, &
        1e-935_xdp, 1e-934_xdp, 1e-933_xdp, 1e-932_xdp, 1e-931_xdp, &
        1e-930_xdp, 1e-929_xdp, 1e-928_xdp, 1e-927_xdp, 1e-926_xdp, &
        1e-925_xdp, 1e-924_xdp, 1e-923_xdp, 1e-922_xdp, 1e-921_xdp, &
        1e-920_xdp, 1e-919_xdp, 1e-918_xdp, 1e-917_xdp, 1e-916_xdp, &
        1e-915_xdp, 1e-914_xdp, 1e-913_xdp, 1e-912_xdp, 1e-911_xdp, &
        1e-910_xdp, 1e-909_xdp, 1e-908_xdp, 1e-907_xdp, 1e-906_xdp, &
        1e-905_xdp, 1e-904_xdp, 1e-903_xdp, 1e-902_xdp, 1e-901_xdp, &
        1e-900_xdp, 1e-899_xdp, 1e-898_xdp, 1e-897_xdp, 1e-896_xdp, &
        1e-895_xdp, 1e-894_xdp, 1e-893_xdp, 1e-892_xdp, 1e-891_xdp, &
        1e-890_xdp, 1e-889_xdp, 1e-888_xdp, 1e-887_xdp, 1e-886_xdp, &
        1e-885_xdp, 1e-884_xdp, 1e-883_xdp, 1e-882_xdp, 1e-881_xdp, &
        1e-880_xdp, 1e-879_xdp, 1e-878_xdp, 1e-877_xdp, 1e-876_xdp, &
        1e-875_xdp, 1e-874_xdp, 1e-873_xdp, 1e-872_xdp, 1e-871_xdp, &
        1e-870_xdp, 1e-869_xdp, 1e-868_xdp, 1e-867_xdp, 1e-866_xdp, &
        1e-865_xdp, 1e-864_xdp, 1e-863_xdp, 1e-862_xdp, 1e-861_xdp, &
        1e-860_xdp, 1e-859_xdp, 1e-858_xdp, 1e-857_xdp, 1e-856_xdp, &
        1e-855_xdp, 1e-854_xdp, 1e-853_xdp, 1e-852_xdp, 1e-851_xdp, &
        1e-850_xdp, 1e-849_xdp, 1e-848_xdp, 1e-847_xdp, 1e-846_xdp, &
        1e-845_xdp, 1e-844_xdp, 1e-843_xdp, 1e-842_xdp, 1e-841_xdp, &
        1e-840_xdp, 1e-839_xdp, 1e-838_xdp, 1e-837_xdp, 1e-836_xdp, &
        1e-835_xdp, 1e-834_xdp, 1e-833_xdp, 1e-832_xdp, 1e-831_xdp, &
        1e-830_xdp, 1e-829_xdp, 1e-828_xdp, 1e-827_xdp, 1e-826_xdp, &
        1e-825_xdp, 1e-824_xdp, 1e-823_xdp, 1e-822_xdp, 1e-821_xdp, &
        1e-820_xdp, 1e-819_xdp, 1e-818_xdp, 1e-817_xdp, 1e-816_xdp, &
        1e-815_xdp, 1e-814_xdp, 1e-813_xdp, 1e-812_xdp, 1e-811_xdp, &
        1e-810_xdp, 1e-809_xdp, 1e-808_xdp, 1e-807_xdp, 1e-806_xdp, &
        1e-805_xdp, 1e-804_xdp, 1e-803_xdp, 1e-802_xdp, 1e-801_xdp, &
        1e-800_xdp, 1e-799_xdp, 1e-798_xdp, 1e-797_xdp, 1e-796_xdp, &
        1e-795_xdp, 1e-794_xdp, 1e-793_xdp, 1e-792_xdp, 1e-791_xdp, &
        1e-790_xdp, 1e-789_xdp, 1e-788_xdp, 1e-787_xdp, 1e-786_xdp, &
        1e-785_xdp, 1e-784_xdp, 1e-783_xdp, 1e-782_xdp, 1e-781_xdp, &
        1e-780_xdp, 1e-779_xdp, 1e-778_xdp, 1e-777_xdp, 1e-776_xdp, &
        1e-775_xdp, 1e-774_xdp, 1e-773_xdp, 1e-772_xdp, 1e-771_xdp, &
        1e-770_xdp, 1e-769_xdp, 1e-768_xdp, 1e-767_xdp, 1e-766_xdp, &
        1e-765_xdp, 1e-764_xdp, 1e-763_xdp, 1e-762_xdp, 1e-761_xdp, &
        1e-760_xdp, 1e-759_xdp, 1e-758_xdp, 1e-757_xdp, 1e-756_xdp, &
        1e-755_xdp, 1e-754_xdp, 1e-753_xdp, 1e-752_xdp, 1e-751_xdp, &
        1e-750_xdp, 1e-749_xdp, 1e-748_xdp, 1e-747_xdp, 1e-746_xdp, &
        1e-745_xdp, 1e-744_xdp, 1e-743_xdp, 1e-742_xdp, 1e-741_xdp, &
        1e-740_xdp, 1e-739_xdp, 1e-738_xdp, 1e-737_xdp, 1e-736_xdp, &
        1e-735_xdp, 1e-734_xdp, 1e-733_xdp, 1e-732_xdp, 1e-731_xdp, &
        1e-730_xdp, 1e-729_xdp, 1e-728_xdp, 1e-727_xdp, 1e-726_xdp, &
        1e-725_xdp, 1e-724_xdp, 1e-723_xdp, 1e-722_xdp, 1e-721_xdp, &
        1e-720_xdp, 1e-719_xdp, 1e-718_xdp, 1e-717_xdp, 1e-716_xdp, &
        1e-715_xdp, 1e-714_xdp, 1e-713_xdp, 1e-712_xdp, 1e-711_xdp, &
        1e-710_xdp, 1e-709_xdp, 1e-708_xdp, 1e-707_xdp, 1e-706_xdp, &
        1e-705_xdp, 1e-704_xdp, 1e-703_xdp, 1e-702_xdp, 1e-701_xdp, &
        1e-700_xdp, 1e-699_xdp, 1e-698_xdp, 1e-697_xdp, 1e-696_xdp, &
        1e-695_xdp, 1e-694_xdp, 1e-693_xdp, 1e-692_xdp, 1e-691_xdp, &
        1e-690_xdp, 1e-689_xdp, 1e-688_xdp, 1e-687_xdp, 1e-686_xdp, &
        1e-685_xdp, 1e-684_xdp, 1e-683_xdp, 1e-682_xdp, 1e-681_xdp, &
        1e-680_xdp, 1e-679_xdp, 1e-678_xdp, 1e-677_xdp, 1e-676_xdp, &
        1e-675_xdp, 1e-674_xdp, 1e-673_xdp, 1e-672_xdp, 1e-671_xdp, &
        1e-670_xdp, 1e-669_xdp, 1e-668_xdp, 1e-667_xdp, 1e-666_xdp, &
        1e-665_xdp, 1e-664_xdp, 1e-663_xdp, 1e-662_xdp, 1e-661_xdp, &
        1e-660_xdp, 1e-659_xdp, 1e-658_xdp, 1e-657_xdp, 1e-656_xdp, &
        1e-655_xdp, 1e-654_xdp, 1e-653_xdp, 1e-652_xdp, 1e-651_xdp, &
        1e-650_xdp, 1e-649_xdp, 1e-648_xdp, 1e-647_xdp, 1e-646_xdp, &
        1e-645_xdp, 1e-644_xdp, 1e-643_xdp, 1e-642_xdp, 1e-641_xdp, &
        1e-640_xdp, 1e-639_xdp, 1e-638_xdp, 1e-637_xdp, 1e-636_xdp, &
        1e-635_xdp, 1e-634_xdp, 1e-633_xdp, 1e-632_xdp, 1e-631_xdp, &
        1e-630_xdp, 1e-629_xdp, 1e-628_xdp, 1e-627_xdp, 1e-626_xdp, &
        1e-625_xdp, 1e-624_xdp, 1e-623_xdp, 1e-622_xdp, 1e-621_xdp, &
        1e-620_xdp, 1e-619_xdp, 1e-618_xdp, 1e-617_xdp, 1e-616_xdp, &
        1e-615_xdp, 1e-614_xdp, 1e-613_xdp, 1e-612_xdp, 1e-611_xdp, &
        1e-610_xdp, 1e-609_xdp, 1e-608_xdp, 1e-607_xdp, 1e-606_xdp, &
        1e-605_xdp, 1e-604_xdp, 1e-603_xdp, 1e-602_xdp, 1e-601_xdp, &
        1e-600_xdp, 1e-599_xdp, 1e-598_xdp, 1e-597_xdp, 1e-596_xdp, &
        1e-595_xdp, 1e-594_xdp, 1e-593_xdp, 1e-592_xdp, 1e-591_xdp, &
        1e-590_xdp, 1e-589_xdp, 1e-588_xdp, 1e-587_xdp, 1e-586_xdp, &
        1e-585_xdp, 1e-584_xdp, 1e-583_xdp, 1e-582_xdp, 1e-581_xdp, &
        1e-580_xdp, 1e-579_xdp, 1e-578_xdp, 1e-577_xdp, 1e-576_xdp, &
        1e-575_xdp, 1e-574_xdp, 1e-573_xdp, 1e-572_xdp, 1e-571_xdp, &
        1e-570_xdp, 1e-569_xdp, 1e-568_xdp, 1e-567_xdp, 1e-566_xdp, &
        1e-565_xdp, 1e-564_xdp, 1e-563_xdp, 1e-562_xdp, 1e-561_xdp, &
        1e-560_xdp, 1e-559_xdp, 1e-558_xdp, 1e-557_xdp, 1e-556_xdp, &
        1e-555_xdp, 1e-554_xdp, 1e-553_xdp, 1e-552_xdp, 1e-551_xdp, &
        1e-550_xdp, 1e-549_xdp, 1e-548_xdp, 1e-547_xdp, 1e-546_xdp, &
        1e-545_xdp, 1e-544_xdp, 1e-543_xdp, 1e-542_xdp, 1e-541_xdp, &
        1e-540_xdp, 1e-539_xdp, 1e-538_xdp, 1e-537_xdp, 1e-536_xdp, &
        1e-535_xdp, 1e-534_xdp, 1e-533_xdp, 1e-532_xdp, 1e-531_xdp, &
        1e-530_xdp, 1e-529_xdp, 1e-528_xdp, 1e-527_xdp, 1e-526_xdp, &
        1e-525_xdp, 1e-524_xdp, 1e-523_xdp, 1e-522_xdp, 1e-521_xdp, &
        1e-520_xdp, 1e-519_xdp, 1e-518_xdp, 1e-517_xdp, 1e-516_xdp, &
        1e-515_xdp, 1e-514_xdp, 1e-513_xdp, 1e-512_xdp, 1e-511_xdp, &
        1e-510_xdp, 1e-509_xdp, 1e-508_xdp, 1e-507_xdp, 1e-506_xdp, &
        1e-505_xdp, 1e-504_xdp, 1e-503_xdp, 1e-502_xdp, 1e-501_xdp, &
        1e-500_xdp, 1e-499_xdp, 1e-498_xdp, 1e-497_xdp, 1e-496_xdp, &
        1e-495_xdp, 1e-494_xdp, 1e-493_xdp, 1e-492_xdp, 1e-491_xdp, &
        1e-490_xdp, 1e-489_xdp, 1e-488_xdp, 1e-487_xdp, 1e-486_xdp, &
        1e-485_xdp, 1e-484_xdp, 1e-483_xdp, 1e-482_xdp, 1e-481_xdp, &
        1e-480_xdp, 1e-479_xdp, 1e-478_xdp, 1e-477_xdp, 1e-476_xdp, &
        1e-475_xdp, 1e-474_xdp, 1e-473_xdp, 1e-472_xdp, 1e-471_xdp, &
        1e-470_xdp, 1e-469_xdp, 1e-468_xdp, 1e-467_xdp, 1e-466_xdp, &
        1e-465_xdp, 1e-464_xdp, 1e-463_xdp, 1e-462_xdp, 1e-461_xdp, &
        1e-460_xdp, 1e-459_xdp, 1e-458_xdp, 1e-457_xdp, 1e-456_xdp, &
        1e-455_xdp, 1e-454_xdp, 1e-453_xdp, 1e-452_xdp, 1e-451_xdp, &
        1e-450_xdp, 1e-449_xdp, 1e-448_xdp, 1e-447_xdp, 1e-446_xdp, &
        1e-445_xdp, 1e-444_xdp, 1e-443_xdp, 1e-442_xdp, 1e-441_xdp, &
        1e-440_xdp, 1e-439_xdp, 1e-438_xdp, 1e-437_xdp, 1e-436_xdp, &
        1e-435_xdp, 1e-434_xdp, 1e-433_xdp, 1e-432_xdp, 1e-431_xdp, &
        1e-430_xdp, 1e-429_xdp, 1e-428_xdp, 1e-427_xdp, 1e-426_xdp, &
        1e-425_xdp, 1e-424_xdp, 1e-423_xdp, 1e-422_xdp, 1e-421_xdp, &
        1e-420_xdp, 1e-419_xdp, 1e-418_xdp, 1e-417_xdp, 1e-416_xdp, &
        1e-415_xdp, 1e-414_xdp, 1e-413_xdp, 1e-412_xdp, 1e-411_xdp, &
        1e-410_xdp, 1e-409_xdp, 1e-408_xdp, 1e-407_xdp, 1e-406_xdp, &
        1e-405_xdp, 1e-404_xdp, 1e-403_xdp, 1e-402_xdp, 1e-401_xdp, &
        1e-400_xdp, 1e-399_xdp, 1e-398_xdp, 1e-397_xdp, 1e-396_xdp, &
        1e-395_xdp, 1e-394_xdp, 1e-393_xdp, 1e-392_xdp, 1e-391_xdp, &
        1e-390_xdp, 1e-389_xdp, 1e-388_xdp, 1e-387_xdp, 1e-386_xdp, &
        1e-385_xdp, 1e-384_xdp, 1e-383_xdp, 1e-382_xdp, 1e-381_xdp, &
        1e-380_xdp, 1e-379_xdp, 1e-378_xdp, 1e-377_xdp, 1e-376_xdp, &
        1e-375_xdp, 1e-374_xdp, 1e-373_xdp, 1e-372_xdp, 1e-371_xdp, &
        1e-370_xdp, 1e-369_xdp, 1e-368_xdp, 1e-367_xdp, 1e-366_xdp, &
        1e-365_xdp, 1e-364_xdp, 1e-363_xdp, 1e-362_xdp, 1e-361_xdp, &
        1e-360_xdp, 1e-359_xdp, 1e-358_xdp, 1e-357_xdp, 1e-356_xdp, &
        1e-355_xdp, 1e-354_xdp, 1e-353_xdp, 1e-352_xdp, 1e-351_xdp, &
        1e-350_xdp, 1e-349_xdp, 1e-348_xdp, 1e-347_xdp, 1e-346_xdp, &
        1e-345_xdp, 1e-344_xdp, 1e-343_xdp, 1e-342_xdp, 1e-341_xdp, &
        1e-340_xdp, 1e-339_xdp, 1e-338_xdp, 1e-337_xdp, 1e-336_xdp, &
        1e-335_xdp, 1e-334_xdp, 1e-333_xdp, 1e-332_xdp, 1e-331_xdp, &
        1e-330_xdp, 1e-329_xdp, 1e-328_xdp, 1e-327_xdp, 1e-326_xdp, &
        1e-325_xdp, 1e-324_xdp, 1e-323_xdp, 1e-322_xdp, 1e-321_xdp, &
        1e-320_xdp, 1e-319_xdp, 1e-318_xdp, 1e-317_xdp, 1e-316_xdp, &
        1e-315_xdp, 1e-314_xdp, 1e-313_xdp, 1e-312_xdp, 1e-311_xdp, &
        1e-310_xdp, 1e-309_xdp, 1e-308_xdp, 1e-307_xdp, 1e-306_xdp, &
        1e-305_xdp, 1e-304_xdp, 1e-303_xdp, 1e-302_xdp, 1e-301_xdp, &
        1e-300_xdp, 1e-299_xdp, 1e-298_xdp, 1e-297_xdp, 1e-296_xdp, &
        1e-295_xdp, 1e-294_xdp, 1e-293_xdp, 1e-292_xdp, 1e-291_xdp, &
        1e-290_xdp, 1e-289_xdp, 1e-288_xdp, 1e-287_xdp, 1e-286_xdp, &
        1e-285_xdp, 1e-284_xdp, 1e-283_xdp, 1e-282_xdp, 1e-281_xdp, &
        1e-280_xdp, 1e-279_xdp, 1e-278_xdp, 1e-277_xdp, 1e-276_xdp, &
        1e-275_xdp, 1e-274_xdp, 1e-273_xdp, 1e-272_xdp, 1e-271_xdp, &
        1e-270_xdp, 1e-269_xdp, 1e-268_xdp, 1e-267_xdp, 1e-266_xdp, &
        1e-265_xdp, 1e-264_xdp, 1e-263_xdp, 1e-262_xdp, 1e-261_xdp, &
        1e-260_xdp, 1e-259_xdp, 1e-258_xdp, 1e-257_xdp, 1e-256_xdp, &
        1e-255_xdp, 1e-254_xdp, 1e-253_xdp, 1e-252_xdp, 1e-251_xdp, &
        1e-250_xdp, 1e-249_xdp, 1e-248_xdp, 1e-247_xdp, 1e-246_xdp, &
        1e-245_xdp, 1e-244_xdp, 1e-243_xdp, 1e-242_xdp, 1e-241_xdp, &
        1e-240_xdp, 1e-239_xdp, 1e-238_xdp, 1e-237_xdp, 1e-236_xdp, &
        1e-235_xdp, 1e-234_xdp, 1e-233_xdp, 1e-232_xdp, 1e-231_xdp, &
        1e-230_xdp, 1e-229_xdp, 1e-228_xdp, 1e-227_xdp, 1e-226_xdp, &
        1e-225_xdp, 1e-224_xdp, 1e-223_xdp, 1e-222_xdp, 1e-221_xdp, &
        1e-220_xdp, 1e-219_xdp, 1e-218_xdp, 1e-217_xdp, 1e-216_xdp, &
        1e-215_xdp, 1e-214_xdp, 1e-213_xdp, 1e-212_xdp, 1e-211_xdp, &
        1e-210_xdp, 1e-209_xdp, 1e-208_xdp, 1e-207_xdp, 1e-206_xdp, &
        1e-205_xdp, 1e-204_xdp, 1e-203_xdp, 1e-202_xdp, 1e-201_xdp, &
        1e-200_xdp, 1e-199_xdp, 1e-198_xdp, 1e-197_xdp, 1e-196_xdp, &
        1e-195_xdp, 1e-194_xdp, 1e-193_xdp, 1e-192_xdp, 1e-191_xdp, &
        1e-190_xdp, 1e-189_xdp, 1e-188_xdp, 1e-187_xdp, 1e-186_xdp, &
        1e-185_xdp, 1e-184_xdp, 1e-183_xdp, 1e-182_xdp, 1e-181_xdp, &
        1e-180_xdp, 1e-179_xdp, 1e-178_xdp, 1e-177_xdp, 1e-176_xdp, &
        1e-175_xdp, 1e-174_xdp, 1e-173_xdp, 1e-172_xdp, 1e-171_xdp, &
        1e-170_xdp, 1e-169_xdp, 1e-168_xdp, 1e-167_xdp, 1e-166_xdp, &
        1e-165_xdp, 1e-164_xdp, 1e-163_xdp, 1e-162_xdp, 1e-161_xdp, &
        1e-160_xdp, 1e-159_xdp, 1e-158_xdp, 1e-157_xdp, 1e-156_xdp, &
        1e-155_xdp, 1e-154_xdp, 1e-153_xdp, 1e-152_xdp, 1e-151_xdp, &
        1e-150_xdp, 1e-149_xdp, 1e-148_xdp, 1e-147_xdp, 1e-146_xdp, &
        1e-145_xdp, 1e-144_xdp, 1e-143_xdp, 1e-142_xdp, 1e-141_xdp, &
        1e-140_xdp, 1e-139_xdp, 1e-138_xdp, 1e-137_xdp, 1e-136_xdp, &
        1e-135_xdp, 1e-134_xdp, 1e-133_xdp, 1e-132_xdp, 1e-131_xdp, &
        1e-130_xdp, 1e-129_xdp, 1e-128_xdp, 1e-127_xdp, 1e-126_xdp, &
        1e-125_xdp, 1e-124_xdp, 1e-123_xdp, 1e-122_xdp, 1e-121_xdp, &
        1e-120_xdp, 1e-119_xdp, 1e-118_xdp, 1e-117_xdp, 1e-116_xdp, &
        1e-115_xdp, 1e-114_xdp, 1e-113_xdp, 1e-112_xdp, 1e-111_xdp, &
        1e-110_xdp, 1e-109_xdp, 1e-108_xdp, 1e-107_xdp, 1e-106_xdp, &
        1e-105_xdp, 1e-104_xdp, 1e-103_xdp, 1e-102_xdp, 1e-101_xdp, &
        1e-100_xdp, 1e-99_xdp, 1e-98_xdp, 1e-97_xdp, 1e-96_xdp, 1e-95_xdp, &
        1e-94_xdp, 1e-93_xdp, 1e-92_xdp, 1e-91_xdp, 1e-90_xdp, 1e-89_xdp, &
        1e-88_xdp, 1e-87_xdp, 1e-86_xdp, 1e-85_xdp, 1e-84_xdp, 1e-83_xdp, &
        1e-82_xdp, 1e-81_xdp, 1e-80_xdp, 1e-79_xdp, 1e-78_xdp, 1e-77_xdp, &
        1e-76_xdp, 1e-75_xdp, 1e-74_xdp, 1e-73_xdp, 1e-72_xdp, 1e-71_xdp, &
        1e-70_xdp, 1e-69_xdp, 1e-68_xdp, 1e-67_xdp, 1e-66_xdp, 1e-65_xdp, &
        1e-64_xdp, 1e-63_xdp, 1e-62_xdp, 1e-61_xdp, 1e-60_xdp, 1e-59_xdp, &
        1e-58_xdp, 1e-57_xdp, 1e-56_xdp, 1e-55_xdp, 1e-54_xdp, 1e-53_xdp, &
        1e-52_xdp, 1e-51_xdp, 1e-50_xdp, 1e-49_xdp, 1e-48_xdp, 1e-47_xdp, &
        1e-46_xdp, 1e-45_xdp, 1e-44_xdp, 1e-43_xdp, 1e-42_xdp, 1e-41_xdp, &
        1e-40_xdp, 1e-39_xdp, 1e-38_xdp, 1e-37_xdp, 1e-36_xdp, 1e-35_xdp, &
        1e-34_xdp, 1e-33_xdp, 1e-32_xdp, 1e-31_xdp, 1e-30_xdp, 1e-29_xdp, &
        1e-28_xdp, 1e-27_xdp, 1e-26_xdp, 1e-25_xdp, 1e-24_xdp, 1e-23_xdp, &
        1e-22_xdp, 1e-21_xdp, 1e-20_xdp, 1e-19_xdp, 1e-18_xdp, 1e-17_xdp, &
        1e-16_xdp, 1e-15_xdp, 1e-14_xdp, 1e-13_xdp, 1e-12_xdp, 1e-11_xdp, &
        1e-10_xdp, 1e-9_xdp, 1e-8_xdp, 1e-7_xdp, 1e-6_xdp, 1e-5_xdp, &
        1e-4_xdp, 1e-3_xdp, 1e-2_xdp, 1e-1_xdp, 1e0_xdp, 1e1_xdp, 1e2_xdp, &
        1e3_xdp, 1e4_xdp, 1e5_xdp, 1e6_xdp, 1e7_xdp, 1e8_xdp, 1e9_xdp, &
        1e10_xdp, 1e11_xdp, 1e12_xdp, 1e13_xdp, 1e14_xdp, 1e15_xdp, 1e16_xdp, &
        1e17_xdp, 1e18_xdp, 1e19_xdp, 1e20_xdp, 1e21_xdp, 1e22_xdp, 1e23_xdp, &
        1e24_xdp, 1e25_xdp, 1e26_xdp, 1e27_xdp, 1e28_xdp, 1e29_xdp, 1e30_xdp, &
        1e31_xdp, 1e32_xdp, 1e33_xdp, 1e34_xdp, 1e35_xdp, 1e36_xdp, 1e37_xdp, &
        1e38_xdp, 1e39_xdp, 1e40_xdp, 1e41_xdp, 1e42_xdp, 1e43_xdp, 1e44_xdp, &
        1e45_xdp, 1e46_xdp, 1e47_xdp, 1e48_xdp, 1e49_xdp, 1e50_xdp, 1e51_xdp, &
        1e52_xdp, 1e53_xdp, 1e54_xdp, 1e55_xdp, 1e56_xdp, 1e57_xdp, 1e58_xdp, &
        1e59_xdp, 1e60_xdp, 1e61_xdp, 1e62_xdp, 1e63_xdp, 1e64_xdp, 1e65_xdp, &
        1e66_xdp, 1e67_xdp, 1e68_xdp, 1e69_xdp, 1e70_xdp, 1e71_xdp, 1e72_xdp, &
        1e73_xdp, 1e74_xdp, 1e75_xdp, 1e76_xdp, 1e77_xdp, 1e78_xdp, 1e79_xdp, &
        1e80_xdp, 1e81_xdp, 1e82_xdp, 1e83_xdp, 1e84_xdp, 1e85_xdp, 1e86_xdp, &
        1e87_xdp, 1e88_xdp, 1e89_xdp, 1e90_xdp, 1e91_xdp, 1e92_xdp, 1e93_xdp, &
        1e94_xdp, 1e95_xdp, 1e96_xdp, 1e97_xdp, 1e98_xdp, 1e99_xdp, &
        1e100_xdp, 1e101_xdp, 1e102_xdp, 1e103_xdp, 1e104_xdp, 1e105_xdp, &
        1e106_xdp, 1e107_xdp, 1e108_xdp, 1e109_xdp, 1e110_xdp, 1e111_xdp, &
        1e112_xdp, 1e113_xdp, 1e114_xdp, 1e115_xdp, 1e116_xdp, 1e117_xdp, &
        1e118_xdp, 1e119_xdp, 1e120_xdp, 1e121_xdp, 1e122_xdp, 1e123_xdp, &
        1e124_xdp, 1e125_xdp, 1e126_xdp, 1e127_xdp, 1e128_xdp, 1e129_xdp, &
        1e130_xdp, 1e131_xdp, 1e132_xdp, 1e133_xdp, 1e134_xdp, 1e135_xdp, &
        1e136_xdp, 1e137_xdp, 1e138_xdp, 1e139_xdp, 1e140_xdp, 1e141_xdp, &
        1e142_xdp, 1e143_xdp, 1e144_xdp, 1e145_xdp, 1e146_xdp, 1e147_xdp, &
        1e148_xdp, 1e149_xdp, 1e150_xdp, 1e151_xdp, 1e152_xdp, 1e153_xdp, &
        1e154_xdp, 1e155_xdp, 1e156_xdp, 1e157_xdp, 1e158_xdp, 1e159_xdp, &
        1e160_xdp, 1e161_xdp, 1e162_xdp, 1e163_xdp, 1e164_xdp, 1e165_xdp, &
        1e166_xdp, 1e167_xdp, 1e168_xdp, 1e169_xdp, 1e170_xdp, 1e171_xdp, &
        1e172_xdp, 1e173_xdp, 1e174_xdp, 1e175_xdp, 1e176_xdp, 1e177_xdp, &
        1e178_xdp, 1e179_xdp, 1e180_xdp, 1e181_xdp, 1e182_xdp, 1e183_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS5(*) = [ &
        1e184_xdp, 1e185_xdp, 1e186_xdp, 1e187_xdp, 1e188_xdp, 1e189_xdp, &
        1e190_xdp, 1e191_xdp, 1e192_xdp, 1e193_xdp, 1e194_xdp, 1e195_xdp, &
        1e196_xdp, 1e197_xdp, 1e198_xdp, 1e199_xdp, 1e200_xdp, 1e201_xdp, &
        1e202_xdp, 1e203_xdp, 1e204_xdp, 1e205_xdp, 1e206_xdp, 1e207_xdp, &
        1e208_xdp, 1e209_xdp, 1e210_xdp, 1e211_xdp, 1e212_xdp, 1e213_xdp, &
        1e214_xdp, 1e215_xdp, 1e216_xdp, 1e217_xdp, 1e218_xdp, 1e219_xdp, &
        1e220_xdp, 1e221_xdp, 1e222_xdp, 1e223_xdp, 1e224_xdp, 1e225_xdp, &
        1e226_xdp, 1e227_xdp, 1e228_xdp, 1e229_xdp, 1e230_xdp, 1e231_xdp, &
        1e232_xdp, 1e233_xdp, 1e234_xdp, 1e235_xdp, 1e236_xdp, 1e237_xdp, &
        1e238_xdp, 1e239_xdp, 1e240_xdp, 1e241_xdp, 1e242_xdp, 1e243_xdp, &
        1e244_xdp, 1e245_xdp, 1e246_xdp, 1e247_xdp, 1e248_xdp, 1e249_xdp, &
        1e250_xdp, 1e251_xdp, 1e252_xdp, 1e253_xdp, 1e254_xdp, 1e255_xdp, &
        1e256_xdp, 1e257_xdp, 1e258_xdp, 1e259_xdp, 1e260_xdp, 1e261_xdp, &
        1e262_xdp, 1e263_xdp, 1e264_xdp, 1e265_xdp, 1e266_xdp, 1e267_xdp, &
        1e268_xdp, 1e269_xdp, 1e270_xdp, 1e271_xdp, 1e272_xdp, 1e273_xdp, &
        1e274_xdp, 1e275_xdp, 1e276_xdp, 1e277_xdp, 1e278_xdp, 1e279_xdp, &
        1e280_xdp, 1e281_xdp, 1e282_xdp, 1e283_xdp, 1e284_xdp, 1e285_xdp, &
        1e286_xdp, 1e287_xdp, 1e288_xdp, 1e289_xdp, 1e290_xdp, 1e291_xdp, &
        1e292_xdp, 1e293_xdp, 1e294_xdp, 1e295_xdp, 1e296_xdp, 1e297_xdp, &
        1e298_xdp, 1e299_xdp, 1e300_xdp, 1e301_xdp, 1e302_xdp, 1e303_xdp, &
        1e304_xdp, 1e305_xdp, 1e306_xdp, 1e307_xdp, 1e308_xdp, 1e309_xdp, &
        1e310_xdp, 1e311_xdp, 1e312_xdp, 1e313_xdp, 1e314_xdp, 1e315_xdp, &
        1e316_xdp, 1e317_xdp, 1e318_xdp, 1e319_xdp, 1e320_xdp, 1e321_xdp, &
        1e322_xdp, 1e323_xdp, 1e324_xdp, 1e325_xdp, 1e326_xdp, 1e327_xdp, &
        1e328_xdp, 1e329_xdp, 1e330_xdp, 1e331_xdp, 1e332_xdp, 1e333_xdp, &
        1e334_xdp, 1e335_xdp, 1e336_xdp, 1e337_xdp, 1e338_xdp, 1e339_xdp, &
        1e340_xdp, 1e341_xdp, 1e342_xdp, 1e343_xdp, 1e344_xdp, 1e345_xdp, &
        1e346_xdp, 1e347_xdp, 1e348_xdp, 1e349_xdp, 1e350_xdp, 1e351_xdp, &
        1e352_xdp, 1e353_xdp, 1e354_xdp, 1e355_xdp, 1e356_xdp, 1e357_xdp, &
        1e358_xdp, 1e359_xdp, 1e360_xdp, 1e361_xdp, 1e362_xdp, 1e363_xdp, &
        1e364_xdp, 1e365_xdp, 1e366_xdp, 1e367_xdp, 1e368_xdp, 1e369_xdp, &
        1e370_xdp, 1e371_xdp, 1e372_xdp, 1e373_xdp, 1e374_xdp, 1e375_xdp, &
        1e376_xdp, 1e377_xdp, 1e378_xdp, 1e379_xdp, 1e380_xdp, 1e381_xdp, &
        1e382_xdp, 1e383_xdp, 1e384_xdp, 1e385_xdp, 1e386_xdp, 1e387_xdp, &
        1e388_xdp, 1e389_xdp, 1e390_xdp, 1e391_xdp, 1e392_xdp, 1e393_xdp, &
        1e394_xdp, 1e395_xdp, 1e396_xdp, 1e397_xdp, 1e398_xdp, 1e399_xdp, &
        1e400_xdp, 1e401_xdp, 1e402_xdp, 1e403_xdp, 1e404_xdp, 1e405_xdp, &
        1e406_xdp, 1e407_xdp, 1e408_xdp, 1e409_xdp, 1e410_xdp, 1e411_xdp, &
        1e412_xdp, 1e413_xdp, 1e414_xdp, 1e415_xdp, 1e416_xdp, 1e417_xdp, &
        1e418_xdp, 1e419_xdp, 1e420_xdp, 1e421_xdp, 1e422_xdp, 1e423_xdp, &
        1e424_xdp, 1e425_xdp, 1e426_xdp, 1e427_xdp, 1e428_xdp, 1e429_xdp, &
        1e430_xdp, 1e431_xdp, 1e432_xdp, 1e433_xdp, 1e434_xdp, 1e435_xdp, &
        1e436_xdp, 1e437_xdp, 1e438_xdp, 1e439_xdp, 1e440_xdp, 1e441_xdp, &
        1e442_xdp, 1e443_xdp, 1e444_xdp, 1e445_xdp, 1e446_xdp, 1e447_xdp, &
        1e448_xdp, 1e449_xdp, 1e450_xdp, 1e451_xdp, 1e452_xdp, 1e453_xdp, &
        1e454_xdp, 1e455_xdp, 1e456_xdp, 1e457_xdp, 1e458_xdp, 1e459_xdp, &
        1e460_xdp, 1e461_xdp, 1e462_xdp, 1e463_xdp, 1e464_xdp, 1e465_xdp, &
        1e466_xdp, 1e467_xdp, 1e468_xdp, 1e469_xdp, 1e470_xdp, 1e471_xdp, &
        1e472_xdp, 1e473_xdp, 1e474_xdp, 1e475_xdp, 1e476_xdp, 1e477_xdp, &
        1e478_xdp, 1e479_xdp, 1e480_xdp, 1e481_xdp, 1e482_xdp, 1e483_xdp, &
        1e484_xdp, 1e485_xdp, 1e486_xdp, 1e487_xdp, 1e488_xdp, 1e489_xdp, &
        1e490_xdp, 1e491_xdp, 1e492_xdp, 1e493_xdp, 1e494_xdp, 1e495_xdp, &
        1e496_xdp, 1e497_xdp, 1e498_xdp, 1e499_xdp, 1e500_xdp, 1e501_xdp, &
        1e502_xdp, 1e503_xdp, 1e504_xdp, 1e505_xdp, 1e506_xdp, 1e507_xdp, &
        1e508_xdp, 1e509_xdp, 1e510_xdp, 1e511_xdp, 1e512_xdp, 1e513_xdp, &
        1e514_xdp, 1e515_xdp, 1e516_xdp, 1e517_xdp, 1e518_xdp, 1e519_xdp, &
        1e520_xdp, 1e521_xdp, 1e522_xdp, 1e523_xdp, 1e524_xdp, 1e525_xdp, &
        1e526_xdp, 1e527_xdp, 1e528_xdp, 1e529_xdp, 1e530_xdp, 1e531_xdp, &
        1e532_xdp, 1e533_xdp, 1e534_xdp, 1e535_xdp, 1e536_xdp, 1e537_xdp, &
        1e538_xdp, 1e539_xdp, 1e540_xdp, 1e541_xdp, 1e542_xdp, 1e543_xdp, &
        1e544_xdp, 1e545_xdp, 1e546_xdp, 1e547_xdp, 1e548_xdp, 1e549_xdp, &
        1e550_xdp, 1e551_xdp, 1e552_xdp, 1e553_xdp, 1e554_xdp, 1e555_xdp, &
        1e556_xdp, 1e557_xdp, 1e558_xdp, 1e559_xdp, 1e560_xdp, 1e561_xdp, &
        1e562_xdp, 1e563_xdp, 1e564_xdp, 1e565_xdp, 1e566_xdp, 1e567_xdp, &
        1e568_xdp, 1e569_xdp, 1e570_xdp, 1e571_xdp, 1e572_xdp, 1e573_xdp, &
        1e574_xdp, 1e575_xdp, 1e576_xdp, 1e577_xdp, 1e578_xdp, 1e579_xdp, &
        1e580_xdp, 1e581_xdp, 1e582_xdp, 1e583_xdp, 1e584_xdp, 1e585_xdp, &
        1e586_xdp, 1e587_xdp, 1e588_xdp, 1e589_xdp, 1e590_xdp, 1e591_xdp, &
        1e592_xdp, 1e593_xdp, 1e594_xdp, 1e595_xdp, 1e596_xdp, 1e597_xdp, &
        1e598_xdp, 1e599_xdp, 1e600_xdp, 1e601_xdp, 1e602_xdp, 1e603_xdp, &
        1e604_xdp, 1e605_xdp, 1e606_xdp, 1e607_xdp, 1e608_xdp, 1e609_xdp, &
        1e610_xdp, 1e611_xdp, 1e612_xdp, 1e613_xdp, 1e614_xdp, 1e615_xdp, &
        1e616_xdp, 1e617_xdp, 1e618_xdp, 1e619_xdp, 1e620_xdp, 1e621_xdp, &
        1e622_xdp, 1e623_xdp, 1e624_xdp, 1e625_xdp, 1e626_xdp, 1e627_xdp, &
        1e628_xdp, 1e629_xdp, 1e630_xdp, 1e631_xdp, 1e632_xdp, 1e633_xdp, &
        1e634_xdp, 1e635_xdp, 1e636_xdp, 1e637_xdp, 1e638_xdp, 1e639_xdp, &
        1e640_xdp, 1e641_xdp, 1e642_xdp, 1e643_xdp, 1e644_xdp, 1e645_xdp, &
        1e646_xdp, 1e647_xdp, 1e648_xdp, 1e649_xdp, 1e650_xdp, 1e651_xdp, &
        1e652_xdp, 1e653_xdp, 1e654_xdp, 1e655_xdp, 1e656_xdp, 1e657_xdp, &
        1e658_xdp, 1e659_xdp, 1e660_xdp, 1e661_xdp, 1e662_xdp, 1e663_xdp, &
        1e664_xdp, 1e665_xdp, 1e666_xdp, 1e667_xdp, 1e668_xdp, 1e669_xdp, &
        1e670_xdp, 1e671_xdp, 1e672_xdp, 1e673_xdp, 1e674_xdp, 1e675_xdp, &
        1e676_xdp, 1e677_xdp, 1e678_xdp, 1e679_xdp, 1e680_xdp, 1e681_xdp, &
        1e682_xdp, 1e683_xdp, 1e684_xdp, 1e685_xdp, 1e686_xdp, 1e687_xdp, &
        1e688_xdp, 1e689_xdp, 1e690_xdp, 1e691_xdp, 1e692_xdp, 1e693_xdp, &
        1e694_xdp, 1e695_xdp, 1e696_xdp, 1e697_xdp, 1e698_xdp, 1e699_xdp, &
        1e700_xdp, 1e701_xdp, 1e702_xdp, 1e703_xdp, 1e704_xdp, 1e705_xdp, &
        1e706_xdp, 1e707_xdp, 1e708_xdp, 1e709_xdp, 1e710_xdp, 1e711_xdp, &
        1e712_xdp, 1e713_xdp, 1e714_xdp, 1e715_xdp, 1e716_xdp, 1e717_xdp, &
        1e718_xdp, 1e719_xdp, 1e720_xdp, 1e721_xdp, 1e722_xdp, 1e723_xdp, &
        1e724_xdp, 1e725_xdp, 1e726_xdp, 1e727_xdp, 1e728_xdp, 1e729_xdp, &
        1e730_xdp, 1e731_xdp, 1e732_xdp, 1e733_xdp, 1e734_xdp, 1e735_xdp, &
        1e736_xdp, 1e737_xdp, 1e738_xdp, 1e739_xdp, 1e740_xdp, 1e741_xdp, &
        1e742_xdp, 1e743_xdp, 1e744_xdp, 1e745_xdp, 1e746_xdp, 1e747_xdp, &
        1e748_xdp, 1e749_xdp, 1e750_xdp, 1e751_xdp, 1e752_xdp, 1e753_xdp, &
        1e754_xdp, 1e755_xdp, 1e756_xdp, 1e757_xdp, 1e758_xdp, 1e759_xdp, &
        1e760_xdp, 1e761_xdp, 1e762_xdp, 1e763_xdp, 1e764_xdp, 1e765_xdp, &
        1e766_xdp, 1e767_xdp, 1e768_xdp, 1e769_xdp, 1e770_xdp, 1e771_xdp, &
        1e772_xdp, 1e773_xdp, 1e774_xdp, 1e775_xdp, 1e776_xdp, 1e777_xdp, &
        1e778_xdp, 1e779_xdp, 1e780_xdp, 1e781_xdp, 1e782_xdp, 1e783_xdp, &
        1e784_xdp, 1e785_xdp, 1e786_xdp, 1e787_xdp, 1e788_xdp, 1e789_xdp, &
        1e790_xdp, 1e791_xdp, 1e792_xdp, 1e793_xdp, 1e794_xdp, 1e795_xdp, &
        1e796_xdp, 1e797_xdp, 1e798_xdp, 1e799_xdp, 1e800_xdp, 1e801_xdp, &
        1e802_xdp, 1e803_xdp, 1e804_xdp, 1e805_xdp, 1e806_xdp, 1e807_xdp, &
        1e808_xdp, 1e809_xdp, 1e810_xdp, 1e811_xdp, 1e812_xdp, 1e813_xdp, &
        1e814_xdp, 1e815_xdp, 1e816_xdp, 1e817_xdp, 1e818_xdp, 1e819_xdp, &
        1e820_xdp, 1e821_xdp, 1e822_xdp, 1e823_xdp, 1e824_xdp, 1e825_xdp, &
        1e826_xdp, 1e827_xdp, 1e828_xdp, 1e829_xdp, 1e830_xdp, 1e831_xdp, &
        1e832_xdp, 1e833_xdp, 1e834_xdp, 1e835_xdp, 1e836_xdp, 1e837_xdp, &
        1e838_xdp, 1e839_xdp, 1e840_xdp, 1e841_xdp, 1e842_xdp, 1e843_xdp, &
        1e844_xdp, 1e845_xdp, 1e846_xdp, 1e847_xdp, 1e848_xdp, 1e849_xdp, &
        1e850_xdp, 1e851_xdp, 1e852_xdp, 1e853_xdp, 1e854_xdp, 1e855_xdp, &
        1e856_xdp, 1e857_xdp, 1e858_xdp, 1e859_xdp, 1e860_xdp, 1e861_xdp, &
        1e862_xdp, 1e863_xdp, 1e864_xdp, 1e865_xdp, 1e866_xdp, 1e867_xdp, &
        1e868_xdp, 1e869_xdp, 1e870_xdp, 1e871_xdp, 1e872_xdp, 1e873_xdp, &
        1e874_xdp, 1e875_xdp, 1e876_xdp, 1e877_xdp, 1e878_xdp, 1e879_xdp, &
        1e880_xdp, 1e881_xdp, 1e882_xdp, 1e883_xdp, 1e884_xdp, 1e885_xdp, &
        1e886_xdp, 1e887_xdp, 1e888_xdp, 1e889_xdp, 1e890_xdp, 1e891_xdp, &
        1e892_xdp, 1e893_xdp, 1e894_xdp, 1e895_xdp, 1e896_xdp, 1e897_xdp, &
        1e898_xdp, 1e899_xdp, 1e900_xdp, 1e901_xdp, 1e902_xdp, 1e903_xdp, &
        1e904_xdp, 1e905_xdp, 1e906_xdp, 1e907_xdp, 1e908_xdp, 1e909_xdp, &
        1e910_xdp, 1e911_xdp, 1e912_xdp, 1e913_xdp, 1e914_xdp, 1e915_xdp, &
        1e916_xdp, 1e917_xdp, 1e918_xdp, 1e919_xdp, 1e920_xdp, 1e921_xdp, &
        1e922_xdp, 1e923_xdp, 1e924_xdp, 1e925_xdp, 1e926_xdp, 1e927_xdp, &
        1e928_xdp, 1e929_xdp, 1e930_xdp, 1e931_xdp, 1e932_xdp, 1e933_xdp, &
        1e934_xdp, 1e935_xdp, 1e936_xdp, 1e937_xdp, 1e938_xdp, 1e939_xdp, &
        1e940_xdp, 1e941_xdp, 1e942_xdp, 1e943_xdp, 1e944_xdp, 1e945_xdp, &
        1e946_xdp, 1e947_xdp, 1e948_xdp, 1e949_xdp, 1e950_xdp, 1e951_xdp, &
        1e952_xdp, 1e953_xdp, 1e954_xdp, 1e955_xdp, 1e956_xdp, 1e957_xdp, &
        1e958_xdp, 1e959_xdp, 1e960_xdp, 1e961_xdp, 1e962_xdp, 1e963_xdp, &
        1e964_xdp, 1e965_xdp, 1e966_xdp, 1e967_xdp, 1e968_xdp, 1e969_xdp, &
        1e970_xdp, 1e971_xdp, 1e972_xdp, 1e973_xdp, 1e974_xdp, 1e975_xdp, &
        1e976_xdp, 1e977_xdp, 1e978_xdp, 1e979_xdp, 1e980_xdp, 1e981_xdp, &
        1e982_xdp, 1e983_xdp, 1e984_xdp, 1e985_xdp, 1e986_xdp, 1e987_xdp, &
        1e988_xdp, 1e989_xdp, 1e990_xdp, 1e991_xdp, 1e992_xdp, 1e993_xdp, &
        1e994_xdp, 1e995_xdp, 1e996_xdp, 1e997_xdp, 1e998_xdp, 1e999_xdp, &
        1e1000_xdp, 1e1001_xdp, 1e1002_xdp, 1e1003_xdp, 1e1004_xdp, &
        1e1005_xdp, 1e1006_xdp, 1e1007_xdp, 1e1008_xdp, 1e1009_xdp, &
        1e1010_xdp, 1e1011_xdp, 1e1012_xdp, 1e1013_xdp, 1e1014_xdp, &
        1e1015_xdp, 1e1016_xdp, 1e1017_xdp, 1e1018_xdp, 1e1019_xdp, &
        1e1020_xdp, 1e1021_xdp, 1e1022_xdp, 1e1023_xdp, 1e1024_xdp, &
        1e1025_xdp, 1e1026_xdp, 1e1027_xdp, 1e1028_xdp, 1e1029_xdp, &
        1e1030_xdp, 1e1031_xdp, 1e1032_xdp, 1e1033_xdp, 1e1034_xdp, &
        1e1035_xdp, 1e1036_xdp, 1e1037_xdp, 1e1038_xdp, 1e1039_xdp, &
        1e1040_xdp, 1e1041_xdp, 1e1042_xdp, 1e1043_xdp, 1e1044_xdp, &
        1e1045_xdp, 1e1046_xdp, 1e1047_xdp, 1e1048_xdp, 1e1049_xdp, &
        1e1050_xdp, 1e1051_xdp, 1e1052_xdp, 1e1053_xdp, 1e1054_xdp, &
        1e1055_xdp, 1e1056_xdp, 1e1057_xdp, 1e1058_xdp, 1e1059_xdp, &
        1e1060_xdp, 1e1061_xdp, 1e1062_xdp, 1e1063_xdp, 1e1064_xdp, &
        1e1065_xdp, 1e1066_xdp, 1e1067_xdp, 1e1068_xdp, 1e1069_xdp, &
        1e1070_xdp, 1e1071_xdp, 1e1072_xdp, 1e1073_xdp, 1e1074_xdp, &
        1e1075_xdp, 1e1076_xdp, 1e1077_xdp, 1e1078_xdp, 1e1079_xdp, &
        1e1080_xdp, 1e1081_xdp, 1e1082_xdp, 1e1083_xdp, 1e1084_xdp, &
        1e1085_xdp, 1e1086_xdp, 1e1087_xdp, 1e1088_xdp, 1e1089_xdp, &
        1e1090_xdp, 1e1091_xdp, 1e1092_xdp, 1e1093_xdp, 1e1094_xdp, &
        1e1095_xdp, 1e1096_xdp, 1e1097_xdp, 1e1098_xdp, 1e1099_xdp, &
        1e1100_xdp, 1e1101_xdp, 1e1102_xdp, 1e1103_xdp, 1e1104_xdp, &
        1e1105_xdp, 1e1106_xdp, 1e1107_xdp, 1e1108_xdp, 1e1109_xdp, &
        1e1110_xdp, 1e1111_xdp, 1e1112_xdp, 1e1113_xdp, 1e1114_xdp, &
        1e1115_xdp, 1e1116_xdp, 1e1117_xdp, 1e1118_xdp, 1e1119_xdp, &
        1e1120_xdp, 1e1121_xdp, 1e1122_xdp, 1e1123_xdp, 1e1124_xdp, &
        1e1125_xdp, 1e1126_xdp, 1e1127_xdp, 1e1128_xdp, 1e1129_xdp, &
        1e1130_xdp, 1e1131_xdp, 1e1132_xdp, 1e1133_xdp, 1e1134_xdp, &
        1e1135_xdp, 1e1136_xdp, 1e1137_xdp, 1e1138_xdp, 1e1139_xdp, &
        1e1140_xdp, 1e1141_xdp, 1e1142_xdp, 1e1143_xdp, 1e1144_xdp, &
        1e1145_xdp, 1e1146_xdp, 1e1147_xdp, 1e1148_xdp, 1e1149_xdp, &
        1e1150_xdp, 1e1151_xdp, 1e1152_xdp, 1e1153_xdp, 1e1154_xdp, &
        1e1155_xdp, 1e1156_xdp, 1e1157_xdp, 1e1158_xdp, 1e1159_xdp, &
        1e1160_xdp, 1e1161_xdp, 1e1162_xdp, 1e1163_xdp, 1e1164_xdp, &
        1e1165_xdp, 1e1166_xdp, 1e1167_xdp, 1e1168_xdp, 1e1169_xdp, &
        1e1170_xdp, 1e1171_xdp, 1e1172_xdp, 1e1173_xdp, 1e1174_xdp, &
        1e1175_xdp, 1e1176_xdp, 1e1177_xdp, 1e1178_xdp, 1e1179_xdp, &
        1e1180_xdp, 1e1181_xdp, 1e1182_xdp, 1e1183_xdp, 1e1184_xdp, &
        1e1185_xdp, 1e1186_xdp, 1e1187_xdp, 1e1188_xdp, 1e1189_xdp, &
        1e1190_xdp, 1e1191_xdp, 1e1192_xdp, 1e1193_xdp, 1e1194_xdp, &
        1e1195_xdp, 1e1196_xdp, 1e1197_xdp, 1e1198_xdp, 1e1199_xdp, &
        1e1200_xdp, 1e1201_xdp, 1e1202_xdp, 1e1203_xdp, 1e1204_xdp, &
        1e1205_xdp, 1e1206_xdp, 1e1207_xdp, 1e1208_xdp, 1e1209_xdp, &
        1e1210_xdp, 1e1211_xdp, 1e1212_xdp, 1e1213_xdp, 1e1214_xdp, &
        1e1215_xdp, 1e1216_xdp, 1e1217_xdp, 1e1218_xdp, 1e1219_xdp, &
        1e1220_xdp, 1e1221_xdp, 1e1222_xdp, 1e1223_xdp, 1e1224_xdp, &
        1e1225_xdp, 1e1226_xdp, 1e1227_xdp, 1e1228_xdp, 1e1229_xdp, &
        1e1230_xdp, 1e1231_xdp, 1e1232_xdp, 1e1233_xdp, 1e1234_xdp, &
        1e1235_xdp, 1e1236_xdp, 1e1237_xdp, 1e1238_xdp, 1e1239_xdp, &
        1e1240_xdp, 1e1241_xdp, 1e1242_xdp, 1e1243_xdp, 1e1244_xdp, &
        1e1245_xdp, 1e1246_xdp, 1e1247_xdp, 1e1248_xdp, 1e1249_xdp, &
        1e1250_xdp, 1e1251_xdp, 1e1252_xdp, 1e1253_xdp, 1e1254_xdp, &
        1e1255_xdp, 1e1256_xdp, 1e1257_xdp, 1e1258_xdp, 1e1259_xdp, &
        1e1260_xdp, 1e1261_xdp, 1e1262_xdp, 1e1263_xdp, 1e1264_xdp, &
        1e1265_xdp, 1e1266_xdp, 1e1267_xdp, 1e1268_xdp, 1e1269_xdp, &
        1e1270_xdp, 1e1271_xdp, 1e1272_xdp, 1e1273_xdp, 1e1274_xdp, &
        1e1275_xdp, 1e1276_xdp, 1e1277_xdp, 1e1278_xdp, 1e1279_xdp, &
        1e1280_xdp, 1e1281_xdp, 1e1282_xdp, 1e1283_xdp, 1e1284_xdp, &
        1e1285_xdp, 1e1286_xdp, 1e1287_xdp, 1e1288_xdp, 1e1289_xdp, &
        1e1290_xdp, 1e1291_xdp, 1e1292_xdp, 1e1293_xdp, 1e1294_xdp, &
        1e1295_xdp, 1e1296_xdp, 1e1297_xdp, 1e1298_xdp, 1e1299_xdp, &
        1e1300_xdp, 1e1301_xdp, 1e1302_xdp, 1e1303_xdp, 1e1304_xdp, &
        1e1305_xdp, 1e1306_xdp, 1e1307_xdp, 1e1308_xdp, 1e1309_xdp, &
        1e1310_xdp, 1e1311_xdp, 1e1312_xdp, 1e1313_xdp, 1e1314_xdp, &
        1e1315_xdp, 1e1316_xdp, 1e1317_xdp, 1e1318_xdp, 1e1319_xdp, &
        1e1320_xdp, 1e1321_xdp, 1e1322_xdp, 1e1323_xdp, 1e1324_xdp, &
        1e1325_xdp, 1e1326_xdp, 1e1327_xdp, 1e1328_xdp, 1e1329_xdp, &
        1e1330_xdp, 1e1331_xdp, 1e1332_xdp, 1e1333_xdp, 1e1334_xdp, &
        1e1335_xdp, 1e1336_xdp, 1e1337_xdp, 1e1338_xdp, 1e1339_xdp, &
        1e1340_xdp, 1e1341_xdp, 1e1342_xdp, 1e1343_xdp, 1e1344_xdp, &
        1e1345_xdp, 1e1346_xdp, 1e1347_xdp, 1e1348_xdp, 1e1349_xdp, &
        1e1350_xdp, 1e1351_xdp, 1e1352_xdp, 1e1353_xdp, 1e1354_xdp, &
        1e1355_xdp, 1e1356_xdp, 1e1357_xdp, 1e1358_xdp, 1e1359_xdp, &
        1e1360_xdp, 1e1361_xdp, 1e1362_xdp, 1e1363_xdp, 1e1364_xdp, &
        1e1365_xdp, 1e1366_xdp, 1e1367_xdp, 1e1368_xdp, 1e1369_xdp, &
        1e1370_xdp, 1e1371_xdp, 1e1372_xdp, 1e1373_xdp, 1e1374_xdp, &
        1e1375_xdp, 1e1376_xdp, 1e1377_xdp, 1e1378_xdp, 1e1379_xdp, &
        1e1380_xdp, 1e1381_xdp, 1e1382_xdp, 1e1383_xdp, 1e1384_xdp, &
        1e1385_xdp, 1e1386_xdp, 1e1387_xdp, 1e1388_xdp, 1e1389_xdp, &
        1e1390_xdp, 1e1391_xdp, 1e1392_xdp, 1e1393_xdp, 1e1394_xdp, &
        1e1395_xdp, 1e1396_xdp, 1e1397_xdp, 1e1398_xdp, 1e1399_xdp, &
        1e1400_xdp, 1e1401_xdp, 1e1402_xdp, 1e1403_xdp, 1e1404_xdp, &
        1e1405_xdp, 1e1406_xdp, 1e1407_xdp, 1e1408_xdp, 1e1409_xdp, &
        1e1410_xdp, 1e1411_xdp, 1e1412_xdp, 1e1413_xdp, 1e1414_xdp, &
        1e1415_xdp, 1e1416_xdp, 1e1417_xdp, 1e1418_xdp, 1e1419_xdp, &
        1e1420_xdp, 1e1421_xdp, 1e1422_xdp, 1e1423_xdp, 1e1424_xdp, &
        1e1425_xdp, 1e1426_xdp, 1e1427_xdp, 1e1428_xdp, 1e1429_xdp, &
        1e1430_xdp, 1e1431_xdp, 1e1432_xdp, 1e1433_xdp, 1e1434_xdp, &
        1e1435_xdp, 1e1436_xdp, 1e1437_xdp, 1e1438_xdp, 1e1439_xdp, &
        1e1440_xdp, 1e1441_xdp, 1e1442_xdp, 1e1443_xdp, 1e1444_xdp, &
        1e1445_xdp, 1e1446_xdp, 1e1447_xdp, 1e1448_xdp, 1e1449_xdp, &
        1e1450_xdp, 1e1451_xdp, 1e1452_xdp, 1e1453_xdp, 1e1454_xdp, &
        1e1455_xdp, 1e1456_xdp, 1e1457_xdp, 1e1458_xdp, 1e1459_xdp, &
        1e1460_xdp, 1e1461_xdp, 1e1462_xdp, 1e1463_xdp, 1e1464_xdp, &
        1e1465_xdp, 1e1466_xdp, 1e1467_xdp, 1e1468_xdp, 1e1469_xdp, &
        1e1470_xdp, 1e1471_xdp, 1e1472_xdp, 1e1473_xdp, 1e1474_xdp, &
        1e1475_xdp, 1e1476_xdp, 1e1477_xdp, 1e1478_xdp, 1e1479_xdp, &
        1e1480_xdp, 1e1481_xdp, 1e1482_xdp, 1e1483_xdp, 1e1484_xdp, &
        1e1485_xdp, 1e1486_xdp, 1e1487_xdp, 1e1488_xdp, 1e1489_xdp, &
        1e1490_xdp, 1e1491_xdp, 1e1492_xdp, 1e1493_xdp, 1e1494_xdp, &
        1e1495_xdp, 1e1496_xdp, 1e1497_xdp, 1e1498_xdp, 1e1499_xdp, &
        1e1500_xdp, 1e1501_xdp, 1e1502_xdp, 1e1503_xdp, 1e1504_xdp, &
        1e1505_xdp, 1e1506_xdp, 1e1507_xdp, 1e1508_xdp, 1e1509_xdp, &
        1e1510_xdp, 1e1511_xdp, 1e1512_xdp, 1e1513_xdp, 1e1514_xdp, &
        1e1515_xdp, 1e1516_xdp, 1e1517_xdp, 1e1518_xdp, 1e1519_xdp, &
        1e1520_xdp, 1e1521_xdp, 1e1522_xdp, 1e1523_xdp, 1e1524_xdp, &
        1e1525_xdp, 1e1526_xdp, 1e1527_xdp, 1e1528_xdp, 1e1529_xdp, &
        1e1530_xdp, 1e1531_xdp, 1e1532_xdp, 1e1533_xdp, 1e1534_xdp, &
        1e1535_xdp, 1e1536_xdp, 1e1537_xdp, 1e1538_xdp, 1e1539_xdp, &
        1e1540_xdp, 1e1541_xdp, 1e1542_xdp, 1e1543_xdp, 1e1544_xdp, &
        1e1545_xdp, 1e1546_xdp, 1e1547_xdp, 1e1548_xdp, 1e1549_xdp, &
        1e1550_xdp, 1e1551_xdp, 1e1552_xdp, 1e1553_xdp, 1e1554_xdp, &
        1e1555_xdp, 1e1556_xdp, 1e1557_xdp, 1e1558_xdp, 1e1559_xdp, &
        1e1560_xdp, 1e1561_xdp, 1e1562_xdp, 1e1563_xdp, 1e1564_xdp, &
        1e1565_xdp, 1e1566_xdp, 1e1567_xdp, 1e1568_xdp, 1e1569_xdp, &
        1e1570_xdp, 1e1571_xdp, 1e1572_xdp, 1e1573_xdp, 1e1574_xdp, &
        1e1575_xdp, 1e1576_xdp, 1e1577_xdp, 1e1578_xdp, 1e1579_xdp, &
        1e1580_xdp, 1e1581_xdp, 1e1582_xdp, 1e1583_xdp, 1e1584_xdp, &
        1e1585_xdp, 1e1586_xdp, 1e1587_xdp, 1e1588_xdp, 1e1589_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS6(*) = [ &
        1e1590_xdp, 1e1591_xdp, 1e1592_xdp, 1e1593_xdp, 1e1594_xdp, &
        1e1595_xdp, 1e1596_xdp, 1e1597_xdp, 1e1598_xdp, 1e1599_xdp, &
        1e1600_xdp, 1e1601_xdp, 1e1602_xdp, 1e1603_xdp, 1e1604_xdp, &
        1e1605_xdp, 1e1606_xdp, 1e1607_xdp, 1e1608_xdp, 1e1609_xdp, &
        1e1610_xdp, 1e1611_xdp, 1e1612_xdp, 1e1613_xdp, 1e1614_xdp, &
        1e1615_xdp, 1e1616_xdp, 1e1617_xdp, 1e1618_xdp, 1e1619_xdp, &
        1e1620_xdp, 1e1621_xdp, 1e1622_xdp, 1e1623_xdp, 1e1624_xdp, &
        1e1625_xdp, 1e1626_xdp, 1e1627_xdp, 1e1628_xdp, 1e1629_xdp, &
        1e1630_xdp, 1e1631_xdp, 1e1632_xdp, 1e1633_xdp, 1e1634_xdp, &
        1e1635_xdp, 1e1636_xdp, 1e1637_xdp, 1e1638_xdp, 1e1639_xdp, &
        1e1640_xdp, 1e1641_xdp, 1e1642_xdp, 1e1643_xdp, 1e1644_xdp, &
        1e1645_xdp, 1e1646_xdp, 1e1647_xdp, 1e1648_xdp, 1e1649_xdp, &
        1e1650_xdp, 1e1651_xdp, 1e1652_xdp, 1e1653_xdp, 1e1654_xdp, &
        1e1655_xdp, 1e1656_xdp, 1e1657_xdp, 1e1658_xdp, 1e1659_xdp, &
        1e1660_xdp, 1e1661_xdp, 1e1662_xdp, 1e1663_xdp, 1e1664_xdp, &
        1e1665_xdp, 1e1666_xdp, 1e1667_xdp, 1e1668_xdp, 1e1669_xdp, &
        1e1670_xdp, 1e1671_xdp, 1e1672_xdp, 1e1673_xdp, 1e1674_xdp, &
        1e1675_xdp, 1e1676_xdp, 1e1677_xdp, 1e1678_xdp, 1e1679_xdp, &
        1e1680_xdp, 1e1681_xdp, 1e1682_xdp, 1e1683_xdp, 1e1684_xdp, &
        1e1685_xdp, 1e1686_xdp, 1e1687_xdp, 1e1688_xdp, 1e1689_xdp, &
        1e1690_xdp, 1e1691_xdp, 1e1692_xdp, 1e1693_xdp, 1e1694_xdp, &
        1e1695_xdp, 1e1696_xdp, 1e1697_xdp, 1e1698_xdp, 1e1699_xdp, &
        1e1700_xdp, 1e1701_xdp, 1e1702_xdp, 1e1703_xdp, 1e1704_xdp, &
        1e1705_xdp, 1e1706_xdp, 1e1707_xdp, 1e1708_xdp, 1e1709_xdp, &
        1e1710_xdp, 1e1711_xdp, 1e1712_xdp, 1e1713_xdp, 1e1714_xdp, &
        1e1715_xdp, 1e1716_xdp, 1e1717_xdp, 1e1718_xdp, 1e1719_xdp, &
        1e1720_xdp, 1e1721_xdp, 1e1722_xdp, 1e1723_xdp, 1e1724_xdp, &
        1e1725_xdp, 1e1726_xdp, 1e1727_xdp, 1e1728_xdp, 1e1729_xdp, &
        1e1730_xdp, 1e1731_xdp, 1e1732_xdp, 1e1733_xdp, 1e1734_xdp, &
        1e1735_xdp, 1e1736_xdp, 1e1737_xdp, 1e1738_xdp, 1e1739_xdp, &
        1e1740_xdp, 1e1741_xdp, 1e1742_xdp, 1e1743_xdp, 1e1744_xdp, &
        1e1745_xdp, 1e1746_xdp, 1e1747_xdp, 1e1748_xdp, 1e1749_xdp, &
        1e1750_xdp, 1e1751_xdp, 1e1752_xdp, 1e1753_xdp, 1e1754_xdp, &
        1e1755_xdp, 1e1756_xdp, 1e1757_xdp, 1e1758_xdp, 1e1759_xdp, &
        1e1760_xdp, 1e1761_xdp, 1e1762_xdp, 1e1763_xdp, 1e1764_xdp, &
        1e1765_xdp, 1e1766_xdp, 1e1767_xdp, 1e1768_xdp, 1e1769_xdp, &
        1e1770_xdp, 1e1771_xdp, 1e1772_xdp, 1e1773_xdp, 1e1774_xdp, &
        1e1775_xdp, 1e1776_xdp, 1e1777_xdp, 1e1778_xdp, 1e1779_xdp, &
        1e1780_xdp, 1e1781_xdp, 1e1782_xdp, 1e1783_xdp, 1e1784_xdp, &
        1e1785_xdp, 1e1786_xdp, 1e1787_xdp, 1e1788_xdp, 1e1789_xdp, &
        1e1790_xdp, 1e1791_xdp, 1e1792_xdp, 1e1793_xdp, 1e1794_xdp, &
        1e1795_xdp, 1e1796_xdp, 1e1797_xdp, 1e1798_xdp, 1e1799_xdp, &
        1e1800_xdp, 1e1801_xdp, 1e1802_xdp, 1e1803_xdp, 1e1804_xdp, &
        1e1805_xdp, 1e1806_xdp, 1e1807_xdp, 1e1808_xdp, 1e1809_xdp, &
        1e1810_xdp, 1e1811_xdp, 1e1812_xdp, 1e1813_xdp, 1e1814_xdp, &
        1e1815_xdp, 1e1816_xdp, 1e1817_xdp, 1e1818_xdp, 1e1819_xdp, &
        1e1820_xdp, 1e1821_xdp, 1e1822_xdp, 1e1823_xdp, 1e1824_xdp, &
        1e1825_xdp, 1e1826_xdp, 1e1827_xdp, 1e1828_xdp, 1e1829_xdp, &
        1e1830_xdp, 1e1831_xdp, 1e1832_xdp, 1e1833_xdp, 1e1834_xdp, &
        1e1835_xdp, 1e1836_xdp, 1e1837_xdp, 1e1838_xdp, 1e1839_xdp, &
        1e1840_xdp, 1e1841_xdp, 1e1842_xdp, 1e1843_xdp, 1e1844_xdp, &
        1e1845_xdp, 1e1846_xdp, 1e1847_xdp, 1e1848_xdp, 1e1849_xdp, &
        1e1850_xdp, 1e1851_xdp, 1e1852_xdp, 1e1853_xdp, 1e1854_xdp, &
        1e1855_xdp, 1e1856_xdp, 1e1857_xdp, 1e1858_xdp, 1e1859_xdp, &
        1e1860_xdp, 1e1861_xdp, 1e1862_xdp, 1e1863_xdp, 1e1864_xdp, &
        1e1865_xdp, 1e1866_xdp, 1e1867_xdp, 1e1868_xdp, 1e1869_xdp, &
        1e1870_xdp, 1e1871_xdp, 1e1872_xdp, 1e1873_xdp, 1e1874_xdp, &
        1e1875_xdp, 1e1876_xdp, 1e1877_xdp, 1e1878_xdp, 1e1879_xdp, &
        1e1880_xdp, 1e1881_xdp, 1e1882_xdp, 1e1883_xdp, 1e1884_xdp, &
        1e1885_xdp, 1e1886_xdp, 1e1887_xdp, 1e1888_xdp, 1e1889_xdp, &
        1e1890_xdp, 1e1891_xdp, 1e1892_xdp, 1e1893_xdp, 1e1894_xdp, &
        1e1895_xdp, 1e1896_xdp, 1e1897_xdp, 1e1898_xdp, 1e1899_xdp, &
        1e1900_xdp, 1e1901_xdp, 1e1902_xdp, 1e1903_xdp, 1e1904_xdp, &
        1e1905_xdp, 1e1906_xdp, 1e1907_xdp, 1e1908_xdp, 1e1909_xdp, &
        1e1910_xdp, 1e1911_xdp, 1e1912_xdp, 1e1913_xdp, 1e1914_xdp, &
        1e1915_xdp, 1e1916_xdp, 1e1917_xdp, 1e1918_xdp, 1e1919_xdp, &
        1e1920_xdp, 1e1921_xdp, 1e1922_xdp, 1e1923_xdp, 1e1924_xdp, &
        1e1925_xdp, 1e1926_xdp, 1e1927_xdp, 1e1928_xdp, 1e1929_xdp, &
        1e1930_xdp, 1e1931_xdp, 1e1932_xdp, 1e1933_xdp, 1e1934_xdp, &
        1e1935_xdp, 1e1936_xdp, 1e1937_xdp, 1e1938_xdp, 1e1939_xdp, &
        1e1940_xdp, 1e1941_xdp, 1e1942_xdp, 1e1943_xdp, 1e1944_xdp, &
        1e1945_xdp, 1e1946_xdp, 1e1947_xdp, 1e1948_xdp, 1e1949_xdp, &
        1e1950_xdp, 1e1951_xdp, 1e1952_xdp, 1e1953_xdp, 1e1954_xdp, &
        1e1955_xdp, 1e1956_xdp, 1e1957_xdp, 1e1958_xdp, 1e1959_xdp, &
        1e1960_xdp, 1e1961_xdp, 1e1962_xdp, 1e1963_xdp, 1e1964_xdp, &
        1e1965_xdp, 1e1966_xdp, 1e1967_xdp, 1e1968_xdp, 1e1969_xdp, &
        1e1970_xdp, 1e1971_xdp, 1e1972_xdp, 1e1973_xdp, 1e1974_xdp, &
        1e1975_xdp, 1e1976_xdp, 1e1977_xdp, 1e1978_xdp, 1e1979_xdp, &
        1e1980_xdp, 1e1981_xdp, 1e1982_xdp, 1e1983_xdp, 1e1984_xdp, &
        1e1985_xdp, 1e1986_xdp, 1e1987_xdp, 1e1988_xdp, 1e1989_xdp, &
        1e1990_xdp, 1e1991_xdp, 1e1992_xdp, 1e1993_xdp, 1e1994_xdp, &
        1e1995_xdp, 1e1996_xdp, 1e1997_xdp, 1e1998_xdp, 1e1999_xdp, &
        1e2000_xdp, 1e2001_xdp, 1e2002_xdp, 1e2003_xdp, 1e2004_xdp, &
        1e2005_xdp, 1e2006_xdp, 1e2007_xdp, 1e2008_xdp, 1e2009_xdp, &
        1e2010_xdp, 1e2011_xdp, 1e2012_xdp, 1e2013_xdp, 1e2014_xdp, &
        1e2015_xdp, 1e2016_xdp, 1e2017_xdp, 1e2018_xdp, 1e2019_xdp, &
        1e2020_xdp, 1e2021_xdp, 1e2022_xdp, 1e2023_xdp, 1e2024_xdp, &
        1e2025_xdp, 1e2026_xdp, 1e2027_xdp, 1e2028_xdp, 1e2029_xdp, &
        1e2030_xdp, 1e2031_xdp, 1e2032_xdp, 1e2033_xdp, 1e2034_xdp, &
        1e2035_xdp, 1e2036_xdp, 1e2037_xdp, 1e2038_xdp, 1e2039_xdp, &
        1e2040_xdp, 1e2041_xdp, 1e2042_xdp, 1e2043_xdp, 1e2044_xdp, &
        1e2045_xdp, 1e2046_xdp, 1e2047_xdp, 1e2048_xdp, 1e2049_xdp, &
        1e2050_xdp, 1e2051_xdp, 1e2052_xdp, 1e2053_xdp, 1e2054_xdp, &
        1e2055_xdp, 1e2056_xdp, 1e2057_xdp, 1e2058_xdp, 1e2059_xdp, &
        1e2060_xdp, 1e2061_xdp, 1e2062_xdp, 1e2063_xdp, 1e2064_xdp, &
        1e2065_xdp, 1e2066_xdp, 1e2067_xdp, 1e2068_xdp, 1e2069_xdp, &
        1e2070_xdp, 1e2071_xdp, 1e2072_xdp, 1e2073_xdp, 1e2074_xdp, &
        1e2075_xdp, 1e2076_xdp, 1e2077_xdp, 1e2078_xdp, 1e2079_xdp, &
        1e2080_xdp, 1e2081_xdp, 1e2082_xdp, 1e2083_xdp, 1e2084_xdp, &
        1e2085_xdp, 1e2086_xdp, 1e2087_xdp, 1e2088_xdp, 1e2089_xdp, &
        1e2090_xdp, 1e2091_xdp, 1e2092_xdp, 1e2093_xdp, 1e2094_xdp, &
        1e2095_xdp, 1e2096_xdp, 1e2097_xdp, 1e2098_xdp, 1e2099_xdp, &
        1e2100_xdp, 1e2101_xdp, 1e2102_xdp, 1e2103_xdp, 1e2104_xdp, &
        1e2105_xdp, 1e2106_xdp, 1e2107_xdp, 1e2108_xdp, 1e2109_xdp, &
        1e2110_xdp, 1e2111_xdp, 1e2112_xdp, 1e2113_xdp, 1e2114_xdp, &
        1e2115_xdp, 1e2116_xdp, 1e2117_xdp, 1e2118_xdp, 1e2119_xdp, &
        1e2120_xdp, 1e2121_xdp, 1e2122_xdp, 1e2123_xdp, 1e2124_xdp, &
        1e2125_xdp, 1e2126_xdp, 1e2127_xdp, 1e2128_xdp, 1e2129_xdp, &
        1e2130_xdp, 1e2131_xdp, 1e2132_xdp, 1e2133_xdp, 1e2134_xdp, &
        1e2135_xdp, 1e2136_xdp, 1e2137_xdp, 1e2138_xdp, 1e2139_xdp, &
        1e2140_xdp, 1e2141_xdp, 1e2142_xdp, 1e2143_xdp, 1e2144_xdp, &
        1e2145_xdp, 1e2146_xdp, 1e2147_xdp, 1e2148_xdp, 1e2149_xdp, &
        1e2150_xdp, 1e2151_xdp, 1e2152_xdp, 1e2153_xdp, 1e2154_xdp, &
        1e2155_xdp, 1e2156_xdp, 1e2157_xdp, 1e2158_xdp, 1e2159_xdp, &
        1e2160_xdp, 1e2161_xdp, 1e2162_xdp, 1e2163_xdp, 1e2164_xdp, &
        1e2165_xdp, 1e2166_xdp, 1e2167_xdp, 1e2168_xdp, 1e2169_xdp, &
        1e2170_xdp, 1e2171_xdp, 1e2172_xdp, 1e2173_xdp, 1e2174_xdp, &
        1e2175_xdp, 1e2176_xdp, 1e2177_xdp, 1e2178_xdp, 1e2179_xdp, &
        1e2180_xdp, 1e2181_xdp, 1e2182_xdp, 1e2183_xdp, 1e2184_xdp, &
        1e2185_xdp, 1e2186_xdp, 1e2187_xdp, 1e2188_xdp, 1e2189_xdp, &
        1e2190_xdp, 1e2191_xdp, 1e2192_xdp, 1e2193_xdp, 1e2194_xdp, &
        1e2195_xdp, 1e2196_xdp, 1e2197_xdp, 1e2198_xdp, 1e2199_xdp, &
        1e2200_xdp, 1e2201_xdp, 1e2202_xdp, 1e2203_xdp, 1e2204_xdp, &
        1e2205_xdp, 1e2206_xdp, 1e2207_xdp, 1e2208_xdp, 1e2209_xdp, &
        1e2210_xdp, 1e2211_xdp, 1e2212_xdp, 1e2213_xdp, 1e2214_xdp, &
        1e2215_xdp, 1e2216_xdp, 1e2217_xdp, 1e2218_xdp, 1e2219_xdp, &
        1e2220_xdp, 1e2221_xdp, 1e2222_xdp, 1e2223_xdp, 1e2224_xdp, &
        1e2225_xdp, 1e2226_xdp, 1e2227_xdp, 1e2228_xdp, 1e2229_xdp, &
        1e2230_xdp, 1e2231_xdp, 1e2232_xdp, 1e2233_xdp, 1e2234_xdp, &
        1e2235_xdp, 1e2236_xdp, 1e2237_xdp, 1e2238_xdp, 1e2239_xdp, &
        1e2240_xdp, 1e2241_xdp, 1e2242_xdp, 1e2243_xdp, 1e2244_xdp, &
        1e2245_xdp, 1e2246_xdp, 1e2247_xdp, 1e2248_xdp, 1e2249_xdp, &
        1e2250_xdp, 1e2251_xdp, 1e2252_xdp, 1e2253_xdp, 1e2254_xdp, &
        1e2255_xdp, 1e2256_xdp, 1e2257_xdp, 1e2258_xdp, 1e2259_xdp, &
        1e2260_xdp, 1e2261_xdp, 1e2262_xdp, 1e2263_xdp, 1e2264_xdp, &
        1e2265_xdp, 1e2266_xdp, 1e2267_xdp, 1e2268_xdp, 1e2269_xdp, &
        1e2270_xdp, 1e2271_xdp, 1e2272_xdp, 1e2273_xdp, 1e2274_xdp, &
        1e2275_xdp, 1e2276_xdp, 1e2277_xdp, 1e2278_xdp, 1e2279_xdp, &
        1e2280_xdp, 1e2281_xdp, 1e2282_xdp, 1e2283_xdp, 1e2284_xdp, &
        1e2285_xdp, 1e2286_xdp, 1e2287_xdp, 1e2288_xdp, 1e2289_xdp, &
        1e2290_xdp, 1e2291_xdp, 1e2292_xdp, 1e2293_xdp, 1e2294_xdp, &
        1e2295_xdp, 1e2296_xdp, 1e2297_xdp, 1e2298_xdp, 1e2299_xdp, &
        1e2300_xdp, 1e2301_xdp, 1e2302_xdp, 1e2303_xdp, 1e2304_xdp, &
        1e2305_xdp, 1e2306_xdp, 1e2307_xdp, 1e2308_xdp, 1e2309_xdp, &
        1e2310_xdp, 1e2311_xdp, 1e2312_xdp, 1e2313_xdp, 1e2314_xdp, &
        1e2315_xdp, 1e2316_xdp, 1e2317_xdp, 1e2318_xdp, 1e2319_xdp, &
        1e2320_xdp, 1e2321_xdp, 1e2322_xdp, 1e2323_xdp, 1e2324_xdp, &
        1e2325_xdp, 1e2326_xdp, 1e2327_xdp, 1e2328_xdp, 1e2329_xdp, &
        1e2330_xdp, 1e2331_xdp, 1e2332_xdp, 1e2333_xdp, 1e2334_xdp, &
        1e2335_xdp, 1e2336_xdp, 1e2337_xdp, 1e2338_xdp, 1e2339_xdp, &
        1e2340_xdp, 1e2341_xdp, 1e2342_xdp, 1e2343_xdp, 1e2344_xdp, &
        1e2345_xdp, 1e2346_xdp, 1e2347_xdp, 1e2348_xdp, 1e2349_xdp, &
        1e2350_xdp, 1e2351_xdp, 1e2352_xdp, 1e2353_xdp, 1e2354_xdp, &
        1e2355_xdp, 1e2356_xdp, 1e2357_xdp, 1e2358_xdp, 1e2359_xdp, &
        1e2360_xdp, 1e2361_xdp, 1e2362_xdp, 1e2363_xdp, 1e2364_xdp, &
        1e2365_xdp, 1e2366_xdp, 1e2367_xdp, 1e2368_xdp, 1e2369_xdp, &
        1e2370_xdp, 1e2371_xdp, 1e2372_xdp, 1e2373_xdp, 1e2374_xdp, &
        1e2375_xdp, 1e2376_xdp, 1e2377_xdp, 1e2378_xdp, 1e2379_xdp, &
        1e2380_xdp, 1e2381_xdp, 1e2382_xdp, 1e2383_xdp, 1e2384_xdp, &
        1e2385_xdp, 1e2386_xdp, 1e2387_xdp, 1e2388_xdp, 1e2389_xdp, &
        1e2390_xdp, 1e2391_xdp, 1e2392_xdp, 1e2393_xdp, 1e2394_xdp, &
        1e2395_xdp, 1e2396_xdp, 1e2397_xdp, 1e2398_xdp, 1e2399_xdp, &
        1e2400_xdp, 1e2401_xdp, 1e2402_xdp, 1e2403_xdp, 1e2404_xdp, &
        1e2405_xdp, 1e2406_xdp, 1e2407_xdp, 1e2408_xdp, 1e2409_xdp, &
        1e2410_xdp, 1e2411_xdp, 1e2412_xdp, 1e2413_xdp, 1e2414_xdp, &
        1e2415_xdp, 1e2416_xdp, 1e2417_xdp, 1e2418_xdp, 1e2419_xdp, &
        1e2420_xdp, 1e2421_xdp, 1e2422_xdp, 1e2423_xdp, 1e2424_xdp, &
        1e2425_xdp, 1e2426_xdp, 1e2427_xdp, 1e2428_xdp, 1e2429_xdp, &
        1e2430_xdp, 1e2431_xdp, 1e2432_xdp, 1e2433_xdp, 1e2434_xdp, &
        1e2435_xdp, 1e2436_xdp, 1e2437_xdp, 1e2438_xdp, 1e2439_xdp, &
        1e2440_xdp, 1e2441_xdp, 1e2442_xdp, 1e2443_xdp, 1e2444_xdp, &
        1e2445_xdp, 1e2446_xdp, 1e2447_xdp, 1e2448_xdp, 1e2449_xdp, &
        1e2450_xdp, 1e2451_xdp, 1e2452_xdp, 1e2453_xdp, 1e2454_xdp, &
        1e2455_xdp, 1e2456_xdp, 1e2457_xdp, 1e2458_xdp, 1e2459_xdp, &
        1e2460_xdp, 1e2461_xdp, 1e2462_xdp, 1e2463_xdp, 1e2464_xdp, &
        1e2465_xdp, 1e2466_xdp, 1e2467_xdp, 1e2468_xdp, 1e2469_xdp, &
        1e2470_xdp, 1e2471_xdp, 1e2472_xdp, 1e2473_xdp, 1e2474_xdp, &
        1e2475_xdp, 1e2476_xdp, 1e2477_xdp, 1e2478_xdp, 1e2479_xdp, &
        1e2480_xdp, 1e2481_xdp, 1e2482_xdp, 1e2483_xdp, 1e2484_xdp, &
        1e2485_xdp, 1e2486_xdp, 1e2487_xdp, 1e2488_xdp, 1e2489_xdp, &
        1e2490_xdp, 1e2491_xdp, 1e2492_xdp, 1e2493_xdp, 1e2494_xdp, &
        1e2495_xdp, 1e2496_xdp, 1e2497_xdp, 1e2498_xdp, 1e2499_xdp, &
        1e2500_xdp, 1e2501_xdp, 1e2502_xdp, 1e2503_xdp, 1e2504_xdp, &
        1e2505_xdp, 1e2506_xdp, 1e2507_xdp, 1e2508_xdp, 1e2509_xdp, &
        1e2510_xdp, 1e2511_xdp, 1e2512_xdp, 1e2513_xdp, 1e2514_xdp, &
        1e2515_xdp, 1e2516_xdp, 1e2517_xdp, 1e2518_xdp, 1e2519_xdp, &
        1e2520_xdp, 1e2521_xdp, 1e2522_xdp, 1e2523_xdp, 1e2524_xdp, &
        1e2525_xdp, 1e2526_xdp, 1e2527_xdp, 1e2528_xdp, 1e2529_xdp, &
        1e2530_xdp, 1e2531_xdp, 1e2532_xdp, 1e2533_xdp, 1e2534_xdp, &
        1e2535_xdp, 1e2536_xdp, 1e2537_xdp, 1e2538_xdp, 1e2539_xdp, &
        1e2540_xdp, 1e2541_xdp, 1e2542_xdp, 1e2543_xdp, 1e2544_xdp, &
        1e2545_xdp, 1e2546_xdp, 1e2547_xdp, 1e2548_xdp, 1e2549_xdp, &
        1e2550_xdp, 1e2551_xdp, 1e2552_xdp, 1e2553_xdp, 1e2554_xdp, &
        1e2555_xdp, 1e2556_xdp, 1e2557_xdp, 1e2558_xdp, 1e2559_xdp, &
        1e2560_xdp, 1e2561_xdp, 1e2562_xdp, 1e2563_xdp, 1e2564_xdp, &
        1e2565_xdp, 1e2566_xdp, 1e2567_xdp, 1e2568_xdp, 1e2569_xdp, &
        1e2570_xdp, 1e2571_xdp, 1e2572_xdp, 1e2573_xdp, 1e2574_xdp, &
        1e2575_xdp, 1e2576_xdp, 1e2577_xdp, 1e2578_xdp, 1e2579_xdp, &
        1e2580_xdp, 1e2581_xdp, 1e2582_xdp, 1e2583_xdp, 1e2584_xdp, &
        1e2585_xdp, 1e2586_xdp, 1e2587_xdp, 1e2588_xdp, 1e2589_xdp, &
        1e2590_xdp, 1e2591_xdp, 1e2592_xdp, 1e2593_xdp, 1e2594_xdp, &
        1e2595_xdp, 1e2596_xdp, 1e2597_xdp, 1e2598_xdp, 1e2599_xdp, &
        1e2600_xdp, 1e2601_xdp, 1e2602_xdp, 1e2603_xdp, 1e2604_xdp, &
        1e2605_xdp, 1e2606_xdp, 1e2607_xdp, 1e2608_xdp, 1e2609_xdp, &
        1e2610_xdp, 1e2611_xdp, 1e2612_xdp, 1e2613_xdp, 1e2614_xdp, &
        1e2615_xdp, 1e2616_xdp, 1e2617_xdp, 1e2618_xdp, 1e2619_xdp, &
        1e2620_xdp, 1e2621_xdp, 1e2622_xdp, 1e2623_xdp, 1e2624_xdp, &
        1e2625_xdp, 1e2626_xdp, 1e2627_xdp, 1e2628_xdp, 1e2629_xdp, &
        1e2630_xdp, 1e2631_xdp, 1e2632_xdp, 1e2633_xdp, 1e2634_xdp, &
        1e2635_xdp, 1e2636_xdp, 1e2637_xdp, 1e2638_xdp, 1e2639_xdp, &
        1e2640_xdp, 1e2641_xdp, 1e2642_xdp, 1e2643_xdp, 1e2644_xdp, &
        1e2645_xdp, 1e2646_xdp, 1e2647_xdp, 1e2648_xdp, 1e2649_xdp, &
        1e2650_xdp, 1e2651_xdp, 1e2652_xdp, 1e2653_xdp, 1e2654_xdp, &
        1e2655_xdp, 1e2656_xdp, 1e2657_xdp, 1e2658_xdp, 1e2659_xdp, &
        1e2660_xdp, 1e2661_xdp, 1e2662_xdp, 1e2663_xdp, 1e2664_xdp, &
        1e2665_xdp, 1e2666_xdp, 1e2667_xdp, 1e2668_xdp, 1e2669_xdp, &
        1e2670_xdp, 1e2671_xdp, 1e2672_xdp, 1e2673_xdp, 1e2674_xdp, &
        1e2675_xdp, 1e2676_xdp, 1e2677_xdp, 1e2678_xdp, 1e2679_xdp, &
        1e2680_xdp, 1e2681_xdp, 1e2682_xdp, 1e2683_xdp, 1e2684_xdp, &
        1e2685_xdp, 1e2686_xdp, 1e2687_xdp, 1e2688_xdp, 1e2689_xdp, &
        1e2690_xdp, 1e2691_xdp, 1e2692_xdp, 1e2693_xdp, 1e2694_xdp, &
        1e2695_xdp, 1e2696_xdp, 1e2697_xdp, 1e2698_xdp, 1e2699_xdp, &
        1e2700_xdp, 1e2701_xdp, 1e2702_xdp, 1e2703_xdp, 1e2704_xdp, &
        1e2705_xdp, 1e2706_xdp, 1e2707_xdp, 1e2708_xdp, 1e2709_xdp, &
        1e2710_xdp, 1e2711_xdp, 1e2712_xdp, 1e2713_xdp, 1e2714_xdp, &
        1e2715_xdp, 1e2716_xdp, 1e2717_xdp, 1e2718_xdp, 1e2719_xdp, &
        1e2720_xdp, 1e2721_xdp, 1e2722_xdp, 1e2723_xdp, 1e2724_xdp, &
        1e2725_xdp, 1e2726_xdp, 1e2727_xdp, 1e2728_xdp, 1e2729_xdp, &
        1e2730_xdp, 1e2731_xdp, 1e2732_xdp, 1e2733_xdp, 1e2734_xdp, &
        1e2735_xdp, 1e2736_xdp, 1e2737_xdp, 1e2738_xdp, 1e2739_xdp, &
        1e2740_xdp, 1e2741_xdp, 1e2742_xdp, 1e2743_xdp, 1e2744_xdp, &
        1e2745_xdp, 1e2746_xdp, 1e2747_xdp, 1e2748_xdp, 1e2749_xdp, &
        1e2750_xdp, 1e2751_xdp, 1e2752_xdp, 1e2753_xdp, 1e2754_xdp, &
        1e2755_xdp, 1e2756_xdp, 1e2757_xdp, 1e2758_xdp, 1e2759_xdp, &
        1e2760_xdp, 1e2761_xdp, 1e2762_xdp, 1e2763_xdp, 1e2764_xdp, &
        1e2765_xdp, 1e2766_xdp, 1e2767_xdp, 1e2768_xdp, 1e2769_xdp, &
        1e2770_xdp, 1e2771_xdp, 1e2772_xdp, 1e2773_xdp, 1e2774_xdp, &
        1e2775_xdp, 1e2776_xdp, 1e2777_xdp, 1e2778_xdp, 1e2779_xdp, &
        1e2780_xdp, 1e2781_xdp, 1e2782_xdp, 1e2783_xdp, 1e2784_xdp, &
        1e2785_xdp, 1e2786_xdp, 1e2787_xdp, 1e2788_xdp, 1e2789_xdp, &
        1e2790_xdp, 1e2791_xdp, 1e2792_xdp, 1e2793_xdp, 1e2794_xdp, &
        1e2795_xdp, 1e2796_xdp, 1e2797_xdp, 1e2798_xdp, 1e2799_xdp, &
        1e2800_xdp, 1e2801_xdp, 1e2802_xdp, 1e2803_xdp, 1e2804_xdp, &
        1e2805_xdp, 1e2806_xdp, 1e2807_xdp, 1e2808_xdp, 1e2809_xdp, &
        1e2810_xdp, 1e2811_xdp, 1e2812_xdp, 1e2813_xdp, 1e2814_xdp, &
        1e2815_xdp, 1e2816_xdp, 1e2817_xdp, 1e2818_xdp, 1e2819_xdp, &
        1e2820_xdp, 1e2821_xdp, 1e2822_xdp, 1e2823_xdp, 1e2824_xdp, &
        1e2825_xdp, 1e2826_xdp, 1e2827_xdp, 1e2828_xdp, 1e2829_xdp, &
        1e2830_xdp, 1e2831_xdp, 1e2832_xdp, 1e2833_xdp, 1e2834_xdp, &
        1e2835_xdp, 1e2836_xdp, 1e2837_xdp, 1e2838_xdp, 1e2839_xdp, &
        1e2840_xdp, 1e2841_xdp, 1e2842_xdp, 1e2843_xdp, 1e2844_xdp, &
        1e2845_xdp, 1e2846_xdp, 1e2847_xdp, 1e2848_xdp, 1e2849_xdp, &
        1e2850_xdp, 1e2851_xdp, 1e2852_xdp, 1e2853_xdp, 1e2854_xdp, &
        1e2855_xdp, 1e2856_xdp, 1e2857_xdp, 1e2858_xdp, 1e2859_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS7(*) = [ &
        1e2860_xdp, 1e2861_xdp, 1e2862_xdp, 1e2863_xdp, 1e2864_xdp, &
        1e2865_xdp, 1e2866_xdp, 1e2867_xdp, 1e2868_xdp, 1e2869_xdp, &
        1e2870_xdp, 1e2871_xdp, 1e2872_xdp, 1e2873_xdp, 1e2874_xdp, &
        1e2875_xdp, 1e2876_xdp, 1e2877_xdp, 1e2878_xdp, 1e2879_xdp, &
        1e2880_xdp, 1e2881_xdp, 1e2882_xdp, 1e2883_xdp, 1e2884_xdp, &
        1e2885_xdp, 1e2886_xdp, 1e2887_xdp, 1e2888_xdp, 1e2889_xdp, &
        1e2890_xdp, 1e2891_xdp, 1e2892_xdp, 1e2893_xdp, 1e2894_xdp, &
        1e2895_xdp, 1e2896_xdp, 1e2897_xdp, 1e2898_xdp, 1e2899_xdp, &
        1e2900_xdp, 1e2901_xdp, 1e2902_xdp, 1e2903_xdp, 1e2904_xdp, &
        1e2905_xdp, 1e2906_xdp, 1e2907_xdp, 1e2908_xdp, 1e2909_xdp, &
        1e2910_xdp, 1e2911_xdp, 1e2912_xdp, 1e2913_xdp, 1e2914_xdp, &
        1e2915_xdp, 1e2916_xdp, 1e2917_xdp, 1e2918_xdp, 1e2919_xdp, &
        1e2920_xdp, 1e2921_xdp, 1e2922_xdp, 1e2923_xdp, 1e2924_xdp, &
        1e2925_xdp, 1e2926_xdp, 1e2927_xdp, 1e2928_xdp, 1e2929_xdp, &
        1e2930_xdp, 1e2931_xdp, 1e2932_xdp, 1e2933_xdp, 1e2934_xdp, &
        1e2935_xdp, 1e2936_xdp, 1e2937_xdp, 1e2938_xdp, 1e2939_xdp, &
        1e2940_xdp, 1e2941_xdp, 1e2942_xdp, 1e2943_xdp, 1e2944_xdp, &
        1e2945_xdp, 1e2946_xdp, 1e2947_xdp, 1e2948_xdp, 1e2949_xdp, &
        1e2950_xdp, 1e2951_xdp, 1e2952_xdp, 1e2953_xdp, 1e2954_xdp, &
        1e2955_xdp, 1e2956_xdp, 1e2957_xdp, 1e2958_xdp, 1e2959_xdp, &
        1e2960_xdp, 1e2961_xdp, 1e2962_xdp, 1e2963_xdp, 1e2964_xdp, &
        1e2965_xdp, 1e2966_xdp, 1e2967_xdp, 1e2968_xdp, 1e2969_xdp, &
        1e2970_xdp, 1e2971_xdp, 1e2972_xdp, 1e2973_xdp, 1e2974_xdp, &
        1e2975_xdp, 1e2976_xdp, 1e2977_xdp, 1e2978_xdp, 1e2979_xdp, &
        1e2980_xdp, 1e2981_xdp, 1e2982_xdp, 1e2983_xdp, 1e2984_xdp, &
        1e2985_xdp, 1e2986_xdp, 1e2987_xdp, 1e2988_xdp, 1e2989_xdp, &
        1e2990_xdp, 1e2991_xdp, 1e2992_xdp, 1e2993_xdp, 1e2994_xdp, &
        1e2995_xdp, 1e2996_xdp, 1e2997_xdp, 1e2998_xdp, 1e2999_xdp, &
        1e3000_xdp, 1e3001_xdp, 1e3002_xdp, 1e3003_xdp, 1e3004_xdp, &
        1e3005_xdp, 1e3006_xdp, 1e3007_xdp, 1e3008_xdp, 1e3009_xdp, &
        1e3010_xdp, 1e3011_xdp, 1e3012_xdp, 1e3013_xdp, 1e3014_xdp, &
        1e3015_xdp, 1e3016_xdp, 1e3017_xdp, 1e3018_xdp, 1e3019_xdp, &
        1e3020_xdp, 1e3021_xdp, 1e3022_xdp, 1e3023_xdp, 1e3024_xdp, &
        1e3025_xdp, 1e3026_xdp, 1e3027_xdp, 1e3028_xdp, 1e3029_xdp, &
        1e3030_xdp, 1e3031_xdp, 1e3032_xdp, 1e3033_xdp, 1e3034_xdp, &
        1e3035_xdp, 1e3036_xdp, 1e3037_xdp, 1e3038_xdp, 1e3039_xdp, &
        1e3040_xdp, 1e3041_xdp, 1e3042_xdp, 1e3043_xdp, 1e3044_xdp, &
        1e3045_xdp, 1e3046_xdp, 1e3047_xdp, 1e3048_xdp, 1e3049_xdp, &
        1e3050_xdp, 1e3051_xdp, 1e3052_xdp, 1e3053_xdp, 1e3054_xdp, &
        1e3055_xdp, 1e3056_xdp, 1e3057_xdp, 1e3058_xdp, 1e3059_xdp, &
        1e3060_xdp, 1e3061_xdp, 1e3062_xdp, 1e3063_xdp, 1e3064_xdp, &
        1e3065_xdp, 1e3066_xdp, 1e3067_xdp, 1e3068_xdp, 1e3069_xdp, &
        1e3070_xdp, 1e3071_xdp, 1e3072_xdp, 1e3073_xdp, 1e3074_xdp, &
        1e3075_xdp, 1e3076_xdp, 1e3077_xdp, 1e3078_xdp, 1e3079_xdp, &
        1e3080_xdp, 1e3081_xdp, 1e3082_xdp, 1e3083_xdp, 1e3084_xdp, &
        1e3085_xdp, 1e3086_xdp, 1e3087_xdp, 1e3088_xdp, 1e3089_xdp, &
        1e3090_xdp, 1e3091_xdp, 1e3092_xdp, 1e3093_xdp, 1e3094_xdp, &
        1e3095_xdp, 1e3096_xdp, 1e3097_xdp, 1e3098_xdp, 1e3099_xdp, &
        1e3100_xdp, 1e3101_xdp, 1e3102_xdp, 1e3103_xdp, 1e3104_xdp, &
        1e3105_xdp, 1e3106_xdp, 1e3107_xdp, 1e3108_xdp, 1e3109_xdp, &
        1e3110_xdp, 1e3111_xdp, 1e3112_xdp, 1e3113_xdp, 1e3114_xdp, &
        1e3115_xdp, 1e3116_xdp, 1e3117_xdp, 1e3118_xdp, 1e3119_xdp, &
        1e3120_xdp, 1e3121_xdp, 1e3122_xdp, 1e3123_xdp, 1e3124_xdp, &
        1e3125_xdp, 1e3126_xdp, 1e3127_xdp, 1e3128_xdp, 1e3129_xdp, &
        1e3130_xdp, 1e3131_xdp, 1e3132_xdp, 1e3133_xdp, 1e3134_xdp, &
        1e3135_xdp, 1e3136_xdp, 1e3137_xdp, 1e3138_xdp, 1e3139_xdp, &
        1e3140_xdp, 1e3141_xdp, 1e3142_xdp, 1e3143_xdp, 1e3144_xdp, &
        1e3145_xdp, 1e3146_xdp, 1e3147_xdp, 1e3148_xdp, 1e3149_xdp, &
        1e3150_xdp, 1e3151_xdp, 1e3152_xdp, 1e3153_xdp, 1e3154_xdp, &
        1e3155_xdp, 1e3156_xdp, 1e3157_xdp, 1e3158_xdp, 1e3159_xdp, &
        1e3160_xdp, 1e3161_xdp, 1e3162_xdp, 1e3163_xdp, 1e3164_xdp, &
        1e3165_xdp, 1e3166_xdp, 1e3167_xdp, 1e3168_xdp, 1e3169_xdp, &
        1e3170_xdp, 1e3171_xdp, 1e3172_xdp, 1e3173_xdp, 1e3174_xdp, &
        1e3175_xdp, 1e3176_xdp, 1e3177_xdp, 1e3178_xdp, 1e3179_xdp, &
        1e3180_xdp, 1e3181_xdp, 1e3182_xdp, 1e3183_xdp, 1e3184_xdp, &
        1e3185_xdp, 1e3186_xdp, 1e3187_xdp, 1e3188_xdp, 1e3189_xdp, &
        1e3190_xdp, 1e3191_xdp, 1e3192_xdp, 1e3193_xdp, 1e3194_xdp, &
        1e3195_xdp, 1e3196_xdp, 1e3197_xdp, 1e3198_xdp, 1e3199_xdp, &
        1e3200_xdp, 1e3201_xdp, 1e3202_xdp, 1e3203_xdp, 1e3204_xdp, &
        1e3205_xdp, 1e3206_xdp, 1e3207_xdp, 1e3208_xdp, 1e3209_xdp, &
        1e3210_xdp, 1e3211_xdp, 1e3212_xdp, 1e3213_xdp, 1e3214_xdp, &
        1e3215_xdp, 1e3216_xdp, 1e3217_xdp, 1e3218_xdp, 1e3219_xdp, &
        1e3220_xdp, 1e3221_xdp, 1e3222_xdp, 1e3223_xdp, 1e3224_xdp, &
        1e3225_xdp, 1e3226_xdp, 1e3227_xdp, 1e3228_xdp, 1e3229_xdp, &
        1e3230_xdp, 1e3231_xdp, 1e3232_xdp, 1e3233_xdp, 1e3234_xdp, &
        1e3235_xdp, 1e3236_xdp, 1e3237_xdp, 1e3238_xdp, 1e3239_xdp, &
        1e3240_xdp, 1e3241_xdp, 1e3242_xdp, 1e3243_xdp, 1e3244_xdp, &
        1e3245_xdp, 1e3246_xdp, 1e3247_xdp, 1e3248_xdp, 1e3249_xdp, &
        1e3250_xdp, 1e3251_xdp, 1e3252_xdp, 1e3253_xdp, 1e3254_xdp, &
        1e3255_xdp, 1e3256_xdp, 1e3257_xdp, 1e3258_xdp, 1e3259_xdp, &
        1e3260_xdp, 1e3261_xdp, 1e3262_xdp, 1e3263_xdp, 1e3264_xdp, &
        1e3265_xdp, 1e3266_xdp, 1e3267_xdp, 1e3268_xdp, 1e3269_xdp, &
        1e3270_xdp, 1e3271_xdp, 1e3272_xdp, 1e3273_xdp, 1e3274_xdp, &
        1e3275_xdp, 1e3276_xdp, 1e3277_xdp, 1e3278_xdp, 1e3279_xdp, &
        1e3280_xdp, 1e3281_xdp, 1e3282_xdp, 1e3283_xdp, 1e3284_xdp, &
        1e3285_xdp, 1e3286_xdp, 1e3287_xdp, 1e3288_xdp, 1e3289_xdp, &
        1e3290_xdp, 1e3291_xdp, 1e3292_xdp, 1e3293_xdp, 1e3294_xdp, &
        1e3295_xdp, 1e3296_xdp, 1e3297_xdp, 1e3298_xdp, 1e3299_xdp, &
        1e3300_xdp, 1e3301_xdp, 1e3302_xdp, 1e3303_xdp, 1e3304_xdp, &
        1e3305_xdp, 1e3306_xdp, 1e3307_xdp, 1e3308_xdp, 1e3309_xdp, &
        1e3310_xdp, 1e3311_xdp, 1e3312_xdp, 1e3313_xdp, 1e3314_xdp, &
        1e3315_xdp, 1e3316_xdp, 1e3317_xdp, 1e3318_xdp, 1e3319_xdp, &
        1e3320_xdp, 1e3321_xdp, 1e3322_xdp, 1e3323_xdp, 1e3324_xdp, &
        1e3325_xdp, 1e3326_xdp, 1e3327_xdp, 1e3328_xdp, 1e3329_xdp, &
        1e3330_xdp, 1e3331_xdp, 1e3332_xdp, 1e3333_xdp, 1e3334_xdp, &
        1e3335_xdp, 1e3336_xdp, 1e3337_xdp, 1e3338_xdp, 1e3339_xdp, &
        1e3340_xdp, 1e3341_xdp, 1e3342_xdp, 1e3343_xdp, 1e3344_xdp, &
        1e3345_xdp, 1e3346_xdp, 1e3347_xdp, 1e3348_xdp, 1e3349_xdp, &
        1e3350_xdp, 1e3351_xdp, 1e3352_xdp, 1e3353_xdp, 1e3354_xdp, &
        1e3355_xdp, 1e3356_xdp, 1e3357_xdp, 1e3358_xdp, 1e3359_xdp, &
        1e3360_xdp, 1e3361_xdp, 1e3362_xdp, 1e3363_xdp, 1e3364_xdp, &
        1e3365_xdp, 1e3366_xdp, 1e3367_xdp, 1e3368_xdp, 1e3369_xdp, &
        1e3370_xdp, 1e3371_xdp, 1e3372_xdp, 1e3373_xdp, 1e3374_xdp, &
        1e3375_xdp, 1e3376_xdp, 1e3377_xdp, 1e3378_xdp, 1e3379_xdp, &
        1e3380_xdp, 1e3381_xdp, 1e3382_xdp, 1e3383_xdp, 1e3384_xdp, &
        1e3385_xdp, 1e3386_xdp, 1e3387_xdp, 1e3388_xdp, 1e3389_xdp, &
        1e3390_xdp, 1e3391_xdp, 1e3392_xdp, 1e3393_xdp, 1e3394_xdp, &
        1e3395_xdp, 1e3396_xdp, 1e3397_xdp, 1e3398_xdp, 1e3399_xdp, &
        1e3400_xdp, 1e3401_xdp, 1e3402_xdp, 1e3403_xdp, 1e3404_xdp, &
        1e3405_xdp, 1e3406_xdp, 1e3407_xdp, 1e3408_xdp, 1e3409_xdp, &
        1e3410_xdp, 1e3411_xdp, 1e3412_xdp, 1e3413_xdp, 1e3414_xdp, &
        1e3415_xdp, 1e3416_xdp, 1e3417_xdp, 1e3418_xdp, 1e3419_xdp, &
        1e3420_xdp, 1e3421_xdp, 1e3422_xdp, 1e3423_xdp, 1e3424_xdp, &
        1e3425_xdp, 1e3426_xdp, 1e3427_xdp, 1e3428_xdp, 1e3429_xdp, &
        1e3430_xdp, 1e3431_xdp, 1e3432_xdp, 1e3433_xdp, 1e3434_xdp, &
        1e3435_xdp, 1e3436_xdp, 1e3437_xdp, 1e3438_xdp, 1e3439_xdp, &
        1e3440_xdp, 1e3441_xdp, 1e3442_xdp, 1e3443_xdp, 1e3444_xdp, &
        1e3445_xdp, 1e3446_xdp, 1e3447_xdp, 1e3448_xdp, 1e3449_xdp, &
        1e3450_xdp, 1e3451_xdp, 1e3452_xdp, 1e3453_xdp, 1e3454_xdp, &
        1e3455_xdp, 1e3456_xdp, 1e3457_xdp, 1e3458_xdp, 1e3459_xdp, &
        1e3460_xdp, 1e3461_xdp, 1e3462_xdp, 1e3463_xdp, 1e3464_xdp, &
        1e3465_xdp, 1e3466_xdp, 1e3467_xdp, 1e3468_xdp, 1e3469_xdp, &
        1e3470_xdp, 1e3471_xdp, 1e3472_xdp, 1e3473_xdp, 1e3474_xdp, &
        1e3475_xdp, 1e3476_xdp, 1e3477_xdp, 1e3478_xdp, 1e3479_xdp, &
        1e3480_xdp, 1e3481_xdp, 1e3482_xdp, 1e3483_xdp, 1e3484_xdp, &
        1e3485_xdp, 1e3486_xdp, 1e3487_xdp, 1e3488_xdp, 1e3489_xdp, &
        1e3490_xdp, 1e3491_xdp, 1e3492_xdp, 1e3493_xdp, 1e3494_xdp, &
        1e3495_xdp, 1e3496_xdp, 1e3497_xdp, 1e3498_xdp, 1e3499_xdp, &
        1e3500_xdp, 1e3501_xdp, 1e3502_xdp, 1e3503_xdp, 1e3504_xdp, &
        1e3505_xdp, 1e3506_xdp, 1e3507_xdp, 1e3508_xdp, 1e3509_xdp, &
        1e3510_xdp, 1e3511_xdp, 1e3512_xdp, 1e3513_xdp, 1e3514_xdp, &
        1e3515_xdp, 1e3516_xdp, 1e3517_xdp, 1e3518_xdp, 1e3519_xdp, &
        1e3520_xdp, 1e3521_xdp, 1e3522_xdp, 1e3523_xdp, 1e3524_xdp, &
        1e3525_xdp, 1e3526_xdp, 1e3527_xdp, 1e3528_xdp, 1e3529_xdp, &
        1e3530_xdp, 1e3531_xdp, 1e3532_xdp, 1e3533_xdp, 1e3534_xdp, &
        1e3535_xdp, 1e3536_xdp, 1e3537_xdp, 1e3538_xdp, 1e3539_xdp, &
        1e3540_xdp, 1e3541_xdp, 1e3542_xdp, 1e3543_xdp, 1e3544_xdp, &
        1e3545_xdp, 1e3546_xdp, 1e3547_xdp, 1e3548_xdp, 1e3549_xdp, &
        1e3550_xdp, 1e3551_xdp, 1e3552_xdp, 1e3553_xdp, 1e3554_xdp, &
        1e3555_xdp, 1e3556_xdp, 1e3557_xdp, 1e3558_xdp, 1e3559_xdp, &
        1e3560_xdp, 1e3561_xdp, 1e3562_xdp, 1e3563_xdp, 1e3564_xdp, &
        1e3565_xdp, 1e3566_xdp, 1e3567_xdp, 1e3568_xdp, 1e3569_xdp, &
        1e3570_xdp, 1e3571_xdp, 1e3572_xdp, 1e3573_xdp, 1e3574_xdp, &
        1e3575_xdp, 1e3576_xdp, 1e3577_xdp, 1e3578_xdp, 1e3579_xdp, &
        1e3580_xdp, 1e3581_xdp, 1e3582_xdp, 1e3583_xdp, 1e3584_xdp, &
        1e3585_xdp, 1e3586_xdp, 1e3587_xdp, 1e3588_xdp, 1e3589_xdp, &
        1e3590_xdp, 1e3591_xdp, 1e3592_xdp, 1e3593_xdp, 1e3594_xdp, &
        1e3595_xdp, 1e3596_xdp, 1e3597_xdp, 1e3598_xdp, 1e3599_xdp, &
        1e3600_xdp, 1e3601_xdp, 1e3602_xdp, 1e3603_xdp, 1e3604_xdp, &
        1e3605_xdp, 1e3606_xdp, 1e3607_xdp, 1e3608_xdp, 1e3609_xdp, &
        1e3610_xdp, 1e3611_xdp, 1e3612_xdp, 1e3613_xdp, 1e3614_xdp, &
        1e3615_xdp, 1e3616_xdp, 1e3617_xdp, 1e3618_xdp, 1e3619_xdp, &
        1e3620_xdp, 1e3621_xdp, 1e3622_xdp, 1e3623_xdp, 1e3624_xdp, &
        1e3625_xdp, 1e3626_xdp, 1e3627_xdp, 1e3628_xdp, 1e3629_xdp, &
        1e3630_xdp, 1e3631_xdp, 1e3632_xdp, 1e3633_xdp, 1e3634_xdp, &
        1e3635_xdp, 1e3636_xdp, 1e3637_xdp, 1e3638_xdp, 1e3639_xdp, &
        1e3640_xdp, 1e3641_xdp, 1e3642_xdp, 1e3643_xdp, 1e3644_xdp, &
        1e3645_xdp, 1e3646_xdp, 1e3647_xdp, 1e3648_xdp, 1e3649_xdp, &
        1e3650_xdp, 1e3651_xdp, 1e3652_xdp, 1e3653_xdp, 1e3654_xdp, &
        1e3655_xdp, 1e3656_xdp, 1e3657_xdp, 1e3658_xdp, 1e3659_xdp, &
        1e3660_xdp, 1e3661_xdp, 1e3662_xdp, 1e3663_xdp, 1e3664_xdp, &
        1e3665_xdp, 1e3666_xdp, 1e3667_xdp, 1e3668_xdp, 1e3669_xdp, &
        1e3670_xdp, 1e3671_xdp, 1e3672_xdp, 1e3673_xdp, 1e3674_xdp, &
        1e3675_xdp, 1e3676_xdp, 1e3677_xdp, 1e3678_xdp, 1e3679_xdp, &
        1e3680_xdp, 1e3681_xdp, 1e3682_xdp, 1e3683_xdp, 1e3684_xdp, &
        1e3685_xdp, 1e3686_xdp, 1e3687_xdp, 1e3688_xdp, 1e3689_xdp, &
        1e3690_xdp, 1e3691_xdp, 1e3692_xdp, 1e3693_xdp, 1e3694_xdp, &
        1e3695_xdp, 1e3696_xdp, 1e3697_xdp, 1e3698_xdp, 1e3699_xdp, &
        1e3700_xdp, 1e3701_xdp, 1e3702_xdp, 1e3703_xdp, 1e3704_xdp, &
        1e3705_xdp, 1e3706_xdp, 1e3707_xdp, 1e3708_xdp, 1e3709_xdp, &
        1e3710_xdp, 1e3711_xdp, 1e3712_xdp, 1e3713_xdp, 1e3714_xdp, &
        1e3715_xdp, 1e3716_xdp, 1e3717_xdp, 1e3718_xdp, 1e3719_xdp, &
        1e3720_xdp, 1e3721_xdp, 1e3722_xdp, 1e3723_xdp, 1e3724_xdp, &
        1e3725_xdp, 1e3726_xdp, 1e3727_xdp, 1e3728_xdp, 1e3729_xdp, &
        1e3730_xdp, 1e3731_xdp, 1e3732_xdp, 1e3733_xdp, 1e3734_xdp, &
        1e3735_xdp, 1e3736_xdp, 1e3737_xdp, 1e3738_xdp, 1e3739_xdp, &
        1e3740_xdp, 1e3741_xdp, 1e3742_xdp, 1e3743_xdp, 1e3744_xdp, &
        1e3745_xdp, 1e3746_xdp, 1e3747_xdp, 1e3748_xdp, 1e3749_xdp, &
        1e3750_xdp, 1e3751_xdp, 1e3752_xdp, 1e3753_xdp, 1e3754_xdp, &
        1e3755_xdp, 1e3756_xdp, 1e3757_xdp, 1e3758_xdp, 1e3759_xdp, &
        1e3760_xdp, 1e3761_xdp, 1e3762_xdp, 1e3763_xdp, 1e3764_xdp, &
        1e3765_xdp, 1e3766_xdp, 1e3767_xdp, 1e3768_xdp, 1e3769_xdp, &
        1e3770_xdp, 1e3771_xdp, 1e3772_xdp, 1e3773_xdp, 1e3774_xdp, &
        1e3775_xdp, 1e3776_xdp, 1e3777_xdp, 1e3778_xdp, 1e3779_xdp, &
        1e3780_xdp, 1e3781_xdp, 1e3782_xdp, 1e3783_xdp, 1e3784_xdp, &
        1e3785_xdp, 1e3786_xdp, 1e3787_xdp, 1e3788_xdp, 1e3789_xdp, &
        1e3790_xdp, 1e3791_xdp, 1e3792_xdp, 1e3793_xdp, 1e3794_xdp, &
        1e3795_xdp, 1e3796_xdp, 1e3797_xdp, 1e3798_xdp, 1e3799_xdp, &
        1e3800_xdp, 1e3801_xdp, 1e3802_xdp, 1e3803_xdp, 1e3804_xdp, &
        1e3805_xdp, 1e3806_xdp, 1e3807_xdp, 1e3808_xdp, 1e3809_xdp, &
        1e3810_xdp, 1e3811_xdp, 1e3812_xdp, 1e3813_xdp, 1e3814_xdp, &
        1e3815_xdp, 1e3816_xdp, 1e3817_xdp, 1e3818_xdp, 1e3819_xdp, &
        1e3820_xdp, 1e3821_xdp, 1e3822_xdp, 1e3823_xdp, 1e3824_xdp, &
        1e3825_xdp, 1e3826_xdp, 1e3827_xdp, 1e3828_xdp, 1e3829_xdp, &
        1e3830_xdp, 1e3831_xdp, 1e3832_xdp, 1e3833_xdp, 1e3834_xdp, &
        1e3835_xdp, 1e3836_xdp, 1e3837_xdp, 1e3838_xdp, 1e3839_xdp, &
        1e3840_xdp, 1e3841_xdp, 1e3842_xdp, 1e3843_xdp, 1e3844_xdp, &
        1e3845_xdp, 1e3846_xdp, 1e3847_xdp, 1e3848_xdp, 1e3849_xdp, &
        1e3850_xdp, 1e3851_xdp, 1e3852_xdp, 1e3853_xdp, 1e3854_xdp, &
        1e3855_xdp, 1e3856_xdp, 1e3857_xdp, 1e3858_xdp, 1e3859_xdp, &
        1e3860_xdp, 1e3861_xdp, 1e3862_xdp, 1e3863_xdp, 1e3864_xdp, &
        1e3865_xdp, 1e3866_xdp, 1e3867_xdp, 1e3868_xdp, 1e3869_xdp, &
        1e3870_xdp, 1e3871_xdp, 1e3872_xdp, 1e3873_xdp, 1e3874_xdp, &
        1e3875_xdp, 1e3876_xdp, 1e3877_xdp, 1e3878_xdp, 1e3879_xdp, &
        1e3880_xdp, 1e3881_xdp, 1e3882_xdp, 1e3883_xdp, 1e3884_xdp, &
        1e3885_xdp, 1e3886_xdp, 1e3887_xdp, 1e3888_xdp, 1e3889_xdp, &
        1e3890_xdp, 1e3891_xdp, 1e3892_xdp, 1e3893_xdp, 1e3894_xdp, &
        1e3895_xdp, 1e3896_xdp, 1e3897_xdp, 1e3898_xdp, 1e3899_xdp, &
        1e3900_xdp, 1e3901_xdp, 1e3902_xdp, 1e3903_xdp, 1e3904_xdp, &
        1e3905_xdp, 1e3906_xdp, 1e3907_xdp, 1e3908_xdp, 1e3909_xdp, &
        1e3910_xdp, 1e3911_xdp, 1e3912_xdp, 1e3913_xdp, 1e3914_xdp, &
        1e3915_xdp, 1e3916_xdp, 1e3917_xdp, 1e3918_xdp, 1e3919_xdp, &
        1e3920_xdp, 1e3921_xdp, 1e3922_xdp, 1e3923_xdp, 1e3924_xdp, &
        1e3925_xdp, 1e3926_xdp, 1e3927_xdp, 1e3928_xdp, 1e3929_xdp, &
        1e3930_xdp, 1e3931_xdp, 1e3932_xdp, 1e3933_xdp, 1e3934_xdp, &
        1e3935_xdp, 1e3936_xdp, 1e3937_xdp, 1e3938_xdp, 1e3939_xdp, &
        1e3940_xdp, 1e3941_xdp, 1e3942_xdp, 1e3943_xdp, 1e3944_xdp, &
        1e3945_xdp, 1e3946_xdp, 1e3947_xdp, 1e3948_xdp, 1e3949_xdp, &
        1e3950_xdp, 1e3951_xdp, 1e3952_xdp, 1e3953_xdp, 1e3954_xdp, &
        1e3955_xdp, 1e3956_xdp, 1e3957_xdp, 1e3958_xdp, 1e3959_xdp, &
        1e3960_xdp, 1e3961_xdp, 1e3962_xdp, 1e3963_xdp, 1e3964_xdp, &
        1e3965_xdp, 1e3966_xdp, 1e3967_xdp, 1e3968_xdp, 1e3969_xdp, &
        1e3970_xdp, 1e3971_xdp, 1e3972_xdp, 1e3973_xdp, 1e3974_xdp, &
        1e3975_xdp, 1e3976_xdp, 1e3977_xdp, 1e3978_xdp, 1e3979_xdp, &
        1e3980_xdp, 1e3981_xdp, 1e3982_xdp, 1e3983_xdp, 1e3984_xdp, &
        1e3985_xdp, 1e3986_xdp, 1e3987_xdp, 1e3988_xdp, 1e3989_xdp, &
        1e3990_xdp, 1e3991_xdp, 1e3992_xdp, 1e3993_xdp, 1e3994_xdp, &
        1e3995_xdp, 1e3996_xdp, 1e3997_xdp, 1e3998_xdp, 1e3999_xdp, &
        1e4000_xdp, 1e4001_xdp, 1e4002_xdp, 1e4003_xdp, 1e4004_xdp, &
        1e4005_xdp, 1e4006_xdp, 1e4007_xdp, 1e4008_xdp, 1e4009_xdp, &
        1e4010_xdp, 1e4011_xdp, 1e4012_xdp, 1e4013_xdp, 1e4014_xdp, &
        1e4015_xdp, 1e4016_xdp, 1e4017_xdp, 1e4018_xdp, 1e4019_xdp, &
        1e4020_xdp, 1e4021_xdp, 1e4022_xdp, 1e4023_xdp, 1e4024_xdp, &
        1e4025_xdp, 1e4026_xdp, 1e4027_xdp, 1e4028_xdp, 1e4029_xdp, &
        1e4030_xdp, 1e4031_xdp, 1e4032_xdp, 1e4033_xdp, 1e4034_xdp, &
        1e4035_xdp, 1e4036_xdp, 1e4037_xdp, 1e4038_xdp, 1e4039_xdp, &
        1e4040_xdp, 1e4041_xdp, 1e4042_xdp, 1e4043_xdp, 1e4044_xdp, &
        1e4045_xdp, 1e4046_xdp, 1e4047_xdp, 1e4048_xdp, 1e4049_xdp, &
        1e4050_xdp, 1e4051_xdp, 1e4052_xdp, 1e4053_xdp, 1e4054_xdp, &
        1e4055_xdp, 1e4056_xdp, 1e4057_xdp, 1e4058_xdp, 1e4059_xdp, &
        1e4060_xdp, 1e4061_xdp, 1e4062_xdp, 1e4063_xdp, 1e4064_xdp, &
        1e4065_xdp, 1e4066_xdp, 1e4067_xdp, 1e4068_xdp, 1e4069_xdp, &
        1e4070_xdp, 1e4071_xdp, 1e4072_xdp, 1e4073_xdp, 1e4074_xdp, &
        1e4075_xdp, 1e4076_xdp, 1e4077_xdp, 1e4078_xdp, 1e4079_xdp, &
        1e4080_xdp, 1e4081_xdp, 1e4082_xdp, 1e4083_xdp, 1e4084_xdp, &
        1e4085_xdp, 1e4086_xdp, 1e4087_xdp, 1e4088_xdp, 1e4089_xdp, &
        1e4090_xdp, 1e4091_xdp, 1e4092_xdp, 1e4093_xdp, 1e4094_xdp, &
        1e4095_xdp, 1e4096_xdp, 1e4097_xdp, 1e4098_xdp, 1e4099_xdp, &
        1e4100_xdp, 1e4101_xdp, 1e4102_xdp, 1e4103_xdp, 1e4104_xdp, &
        1e4105_xdp, 1e4106_xdp, 1e4107_xdp, 1e4108_xdp, 1e4109_xdp, &
        1e4110_xdp, 1e4111_xdp, 1e4112_xdp, 1e4113_xdp, 1e4114_xdp, &
        1e4115_xdp, 1e4116_xdp, 1e4117_xdp, 1e4118_xdp, 1e4119_xdp, &
        1e4120_xdp, 1e4121_xdp, 1e4122_xdp, 1e4123_xdp, 1e4124_xdp, &
        1e4125_xdp, 1e4126_xdp, 1e4127_xdp, 1e4128_xdp, 1e4129_xdp &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS8(*) = [ &
        1e4130_xdp, 1e4131_xdp, 1e4132_xdp, 1e4133_xdp, 1e4134_xdp, &
        1e4135_xdp, 1e4136_xdp, 1e4137_xdp, 1e4138_xdp, 1e4139_xdp, &
        1e4140_xdp, 1e4141_xdp, 1e4142_xdp, 1e4143_xdp, 1e4144_xdp, &
        1e4145_xdp, 1e4146_xdp, 1e4147_xdp, 1e4148_xdp, 1e4149_xdp, &
        1e4150_xdp, 1e4151_xdp, 1e4152_xdp, 1e4153_xdp, 1e4154_xdp, &
        1e4155_xdp, 1e4156_xdp, 1e4157_xdp, 1e4158_xdp, 1e4159_xdp, &
        1e4160_xdp, 1e4161_xdp, 1e4162_xdp, 1e4163_xdp, 1e4164_xdp, &
        1e4165_xdp, 1e4166_xdp, 1e4167_xdp, 1e4168_xdp, 1e4169_xdp, &
        1e4170_xdp, 1e4171_xdp, 1e4172_xdp, 1e4173_xdp, 1e4174_xdp, &
        1e4175_xdp, 1e4176_xdp, 1e4177_xdp, 1e4178_xdp, 1e4179_xdp, &
        1e4180_xdp, 1e4181_xdp, 1e4182_xdp, 1e4183_xdp, 1e4184_xdp, &
        1e4185_xdp, 1e4186_xdp, 1e4187_xdp, 1e4188_xdp, 1e4189_xdp, &
        1e4190_xdp, 1e4191_xdp, 1e4192_xdp, 1e4193_xdp, 1e4194_xdp, &
        1e4195_xdp, 1e4196_xdp, 1e4197_xdp, 1e4198_xdp, 1e4199_xdp, &
        1e4200_xdp, 1e4201_xdp, 1e4202_xdp, 1e4203_xdp, 1e4204_xdp, &
        1e4205_xdp, 1e4206_xdp, 1e4207_xdp, 1e4208_xdp, 1e4209_xdp, &
        1e4210_xdp, 1e4211_xdp, 1e4212_xdp, 1e4213_xdp, 1e4214_xdp, &
        1e4215_xdp, 1e4216_xdp, 1e4217_xdp, 1e4218_xdp, 1e4219_xdp, &
        1e4220_xdp, 1e4221_xdp, 1e4222_xdp, 1e4223_xdp, 1e4224_xdp, &
        1e4225_xdp, 1e4226_xdp, 1e4227_xdp, 1e4228_xdp, 1e4229_xdp, &
        1e4230_xdp, 1e4231_xdp, 1e4232_xdp, 1e4233_xdp, 1e4234_xdp, &
        1e4235_xdp, 1e4236_xdp, 1e4237_xdp, 1e4238_xdp, 1e4239_xdp, &
        1e4240_xdp, 1e4241_xdp, 1e4242_xdp, 1e4243_xdp, 1e4244_xdp, &
        1e4245_xdp, 1e4246_xdp, 1e4247_xdp, 1e4248_xdp, 1e4249_xdp, &
        1e4250_xdp, 1e4251_xdp, 1e4252_xdp, 1e4253_xdp, 1e4254_xdp, &
        1e4255_xdp, 1e4256_xdp, 1e4257_xdp, 1e4258_xdp, 1e4259_xdp, &
        1e4260_xdp, 1e4261_xdp, 1e4262_xdp, 1e4263_xdp, 1e4264_xdp, &
        1e4265_xdp, 1e4266_xdp, 1e4267_xdp, 1e4268_xdp, 1e4269_xdp, &
        1e4270_xdp, 1e4271_xdp, 1e4272_xdp, 1e4273_xdp, 1e4274_xdp, &
        1e4275_xdp, 1e4276_xdp, 1e4277_xdp, 1e4278_xdp, 1e4279_xdp, &
        1e4280_xdp, 1e4281_xdp, 1e4282_xdp, 1e4283_xdp, 1e4284_xdp, &
        1e4285_xdp, 1e4286_xdp, 1e4287_xdp, 1e4288_xdp, 1e4289_xdp, &
        1e4290_xdp, 1e4291_xdp, 1e4292_xdp, 1e4293_xdp, 1e4294_xdp, &
        1e4295_xdp, 1e4296_xdp, 1e4297_xdp, 1e4298_xdp, 1e4299_xdp, &
        1e4300_xdp, 1e4301_xdp, 1e4302_xdp, 1e4303_xdp, 1e4304_xdp, &
        1e4305_xdp, 1e4306_xdp, 1e4307_xdp, 1e4308_xdp, 1e4309_xdp, &
        1e4310_xdp, 1e4311_xdp, 1e4312_xdp, 1e4313_xdp, 1e4314_xdp, &
        1e4315_xdp, 1e4316_xdp, 1e4317_xdp, 1e4318_xdp, 1e4319_xdp, &
        1e4320_xdp, 1e4321_xdp, 1e4322_xdp, 1e4323_xdp, 1e4324_xdp, &
        1e4325_xdp, 1e4326_xdp, 1e4327_xdp, 1e4328_xdp, 1e4329_xdp, &
        1e4330_xdp, 1e4331_xdp, 1e4332_xdp, 1e4333_xdp, 1e4334_xdp, &
        1e4335_xdp, 1e4336_xdp, 1e4337_xdp, 1e4338_xdp, 1e4339_xdp, &
        1e4340_xdp, 1e4341_xdp, 1e4342_xdp, 1e4343_xdp, 1e4344_xdp, &
        1e4345_xdp, 1e4346_xdp, 1e4347_xdp, 1e4348_xdp, 1e4349_xdp, &
        1e4350_xdp, 1e4351_xdp, 1e4352_xdp, 1e4353_xdp, 1e4354_xdp, &
        1e4355_xdp, 1e4356_xdp, 1e4357_xdp, 1e4358_xdp, 1e4359_xdp, &
        1e4360_xdp, 1e4361_xdp, 1e4362_xdp, 1e4363_xdp, 1e4364_xdp, &
        1e4365_xdp, 1e4366_xdp, 1e4367_xdp, 1e4368_xdp, 1e4369_xdp, &
        1e4370_xdp, 1e4371_xdp, 1e4372_xdp, 1e4373_xdp, 1e4374_xdp, &
        1e4375_xdp, 1e4376_xdp, 1e4377_xdp, 1e4378_xdp, 1e4379_xdp, &
        1e4380_xdp, 1e4381_xdp, 1e4382_xdp, 1e4383_xdp, 1e4384_xdp, &
        1e4385_xdp, 1e4386_xdp, 1e4387_xdp, 1e4388_xdp, 1e4389_xdp, &
        1e4390_xdp, 1e4391_xdp, 1e4392_xdp, 1e4393_xdp, 1e4394_xdp, &
        1e4395_xdp, 1e4396_xdp, 1e4397_xdp, 1e4398_xdp, 1e4399_xdp, &
        1e4400_xdp, 1e4401_xdp, 1e4402_xdp, 1e4403_xdp, 1e4404_xdp, &
        1e4405_xdp, 1e4406_xdp, 1e4407_xdp, 1e4408_xdp, 1e4409_xdp, &
        1e4410_xdp, 1e4411_xdp, 1e4412_xdp, 1e4413_xdp, 1e4414_xdp, &
        1e4415_xdp, 1e4416_xdp, 1e4417_xdp, 1e4418_xdp, 1e4419_xdp, &
        1e4420_xdp, 1e4421_xdp, 1e4422_xdp, 1e4423_xdp, 1e4424_xdp, &
        1e4425_xdp, 1e4426_xdp, 1e4427_xdp, 1e4428_xdp, 1e4429_xdp, &
        1e4430_xdp, 1e4431_xdp, 1e4432_xdp, 1e4433_xdp, 1e4434_xdp, &
        1e4435_xdp, 1e4436_xdp, 1e4437_xdp, 1e4438_xdp, 1e4439_xdp, &
        1e4440_xdp, 1e4441_xdp, 1e4442_xdp, 1e4443_xdp, 1e4444_xdp, &
        1e4445_xdp, 1e4446_xdp, 1e4447_xdp, 1e4448_xdp, 1e4449_xdp, &
        1e4450_xdp, 1e4451_xdp, 1e4452_xdp, 1e4453_xdp, 1e4454_xdp, &
        1e4455_xdp, 1e4456_xdp, 1e4457_xdp, 1e4458_xdp, 1e4459_xdp, &
        1e4460_xdp, 1e4461_xdp, 1e4462_xdp, 1e4463_xdp, 1e4464_xdp, &
        1e4465_xdp, 1e4466_xdp, 1e4467_xdp, 1e4468_xdp, 1e4469_xdp, &
        1e4470_xdp, 1e4471_xdp, 1e4472_xdp, 1e4473_xdp, 1e4474_xdp, &
        1e4475_xdp, 1e4476_xdp, 1e4477_xdp, 1e4478_xdp, 1e4479_xdp, &
        1e4480_xdp, 1e4481_xdp, 1e4482_xdp, 1e4483_xdp, 1e4484_xdp, &
        1e4485_xdp, 1e4486_xdp, 1e4487_xdp, 1e4488_xdp, 1e4489_xdp, &
        1e4490_xdp, 1e4491_xdp, 1e4492_xdp, 1e4493_xdp, 1e4494_xdp, &
        1e4495_xdp, 1e4496_xdp, 1e4497_xdp, 1e4498_xdp, 1e4499_xdp, &
        1e4500_xdp, 1e4501_xdp, 1e4502_xdp, 1e4503_xdp, 1e4504_xdp, &
        1e4505_xdp, 1e4506_xdp, 1e4507_xdp, 1e4508_xdp, 1e4509_xdp, &
        1e4510_xdp, 1e4511_xdp, 1e4512_xdp, 1e4513_xdp, 1e4514_xdp, &
        1e4515_xdp, 1e4516_xdp, 1e4517_xdp, 1e4518_xdp, 1e4519_xdp, &
        1e4520_xdp, 1e4521_xdp, 1e4522_xdp, 1e4523_xdp, 1e4524_xdp, &
        1e4525_xdp, 1e4526_xdp, 1e4527_xdp, 1e4528_xdp, 1e4529_xdp, &
        1e4530_xdp, 1e4531_xdp, 1e4532_xdp, 1e4533_xdp, 1e4534_xdp, &
        1e4535_xdp, 1e4536_xdp, 1e4537_xdp, 1e4538_xdp, 1e4539_xdp, &
        1e4540_xdp, 1e4541_xdp, 1e4542_xdp, 1e4543_xdp, 1e4544_xdp, &
        1e4545_xdp, 1e4546_xdp, 1e4547_xdp, 1e4548_xdp, 1e4549_xdp, &
        1e4550_xdp, 1e4551_xdp, 1e4552_xdp, 1e4553_xdp, 1e4554_xdp, &
        1e4555_xdp, 1e4556_xdp, 1e4557_xdp, 1e4558_xdp, 1e4559_xdp, &
        1e4560_xdp, 1e4561_xdp, 1e4562_xdp, 1e4563_xdp, 1e4564_xdp, &
        1e4565_xdp, 1e4566_xdp, 1e4567_xdp, 1e4568_xdp, 1e4569_xdp, &
        1e4570_xdp, 1e4571_xdp, 1e4572_xdp, 1e4573_xdp, 1e4574_xdp, &
        1e4575_xdp, 1e4576_xdp, 1e4577_xdp, 1e4578_xdp, 1e4579_xdp, &
        1e4580_xdp, 1e4581_xdp, 1e4582_xdp, 1e4583_xdp, 1e4584_xdp, &
        1e4585_xdp, 1e4586_xdp, 1e4587_xdp, 1e4588_xdp, 1e4589_xdp, &
        1e4590_xdp, 1e4591_xdp, 1e4592_xdp, 1e4593_xdp, 1e4594_xdp, &
        1e4595_xdp, 1e4596_xdp, 1e4597_xdp, 1e4598_xdp, 1e4599_xdp, &
        1e4600_xdp, 1e4601_xdp, 1e4602_xdp, 1e4603_xdp, 1e4604_xdp, &
        1e4605_xdp, 1e4606_xdp, 1e4607_xdp, 1e4608_xdp, 1e4609_xdp, &
        1e4610_xdp, 1e4611_xdp, 1e4612_xdp, 1e4613_xdp, 1e4614_xdp, &
        1e4615_xdp, 1e4616_xdp, 1e4617_xdp, 1e4618_xdp, 1e4619_xdp, &
        1e4620_xdp, 1e4621_xdp, 1e4622_xdp, 1e4623_xdp, 1e4624_xdp, &
        1e4625_xdp, 1e4626_xdp, 1e4627_xdp, 1e4628_xdp, 1e4629_xdp, &
        1e4630_xdp, 1e4631_xdp, 1e4632_xdp, 1e4633_xdp, 1e4634_xdp, &
        1e4635_xdp, 1e4636_xdp, 1e4637_xdp, 1e4638_xdp, 1e4639_xdp, &
        1e4640_xdp, 1e4641_xdp, 1e4642_xdp, 1e4643_xdp, 1e4644_xdp, &
        1e4645_xdp, 1e4646_xdp, 1e4647_xdp, 1e4648_xdp, 1e4649_xdp, &
        1e4650_xdp, 1e4651_xdp, 1e4652_xdp, 1e4653_xdp, 1e4654_xdp, &
        1e4655_xdp, 1e4656_xdp, 1e4657_xdp, 1e4658_xdp, 1e4659_xdp, &
        1e4660_xdp, 1e4661_xdp, 1e4662_xdp, 1e4663_xdp, 1e4664_xdp, &
        1e4665_xdp, 1e4666_xdp, 1e4667_xdp, 1e4668_xdp, 1e4669_xdp, &
        1e4670_xdp, 1e4671_xdp, 1e4672_xdp, 1e4673_xdp, 1e4674_xdp, &
        1e4675_xdp, 1e4676_xdp, 1e4677_xdp, 1e4678_xdp, 1e4679_xdp, &
        1e4680_xdp, 1e4681_xdp, 1e4682_xdp, 1e4683_xdp, 1e4684_xdp, &
        1e4685_xdp, 1e4686_xdp, 1e4687_xdp, 1e4688_xdp, 1e4689_xdp, &
        1e4690_xdp, 1e4691_xdp, 1e4692_xdp, 1e4693_xdp, 1e4694_xdp, &
        1e4695_xdp, 1e4696_xdp, 1e4697_xdp, 1e4698_xdp, 1e4699_xdp, &
        1e4700_xdp, 1e4701_xdp, 1e4702_xdp, 1e4703_xdp, 1e4704_xdp, &
        1e4705_xdp, 1e4706_xdp, 1e4707_xdp, 1e4708_xdp, 1e4709_xdp, &
        1e4710_xdp, 1e4711_xdp, 1e4712_xdp, 1e4713_xdp, 1e4714_xdp, &
        1e4715_xdp, 1e4716_xdp, 1e4717_xdp, 1e4718_xdp, 1e4719_xdp, &
        1e4720_xdp, 1e4721_xdp, 1e4722_xdp, 1e4723_xdp, 1e4724_xdp, &
        1e4725_xdp, 1e4726_xdp, 1e4727_xdp, 1e4728_xdp, 1e4729_xdp, &
        1e4730_xdp, 1e4731_xdp, 1e4732_xdp, 1e4733_xdp, 1e4734_xdp, &
        1e4735_xdp, 1e4736_xdp, 1e4737_xdp, 1e4738_xdp, 1e4739_xdp, &
        1e4740_xdp, 1e4741_xdp, 1e4742_xdp, 1e4743_xdp, 1e4744_xdp, &
        1e4745_xdp, 1e4746_xdp, 1e4747_xdp, 1e4748_xdp, 1e4749_xdp, &
        1e4750_xdp, 1e4751_xdp, 1e4752_xdp, 1e4753_xdp, 1e4754_xdp, &
        1e4755_xdp, 1e4756_xdp, 1e4757_xdp, 1e4758_xdp, 1e4759_xdp, &
        1e4760_xdp, 1e4761_xdp, 1e4762_xdp, 1e4763_xdp, 1e4764_xdp, &
        1e4765_xdp, 1e4766_xdp, 1e4767_xdp, 1e4768_xdp, 1e4769_xdp, &
        1e4770_xdp, 1e4771_xdp, 1e4772_xdp, 1e4773_xdp, 1e4774_xdp, &
        1e4775_xdp, 1e4776_xdp, 1e4777_xdp, 1e4778_xdp, 1e4779_xdp, &
        1e4780_xdp, 1e4781_xdp, 1e4782_xdp, 1e4783_xdp, 1e4784_xdp, &
        1e4785_xdp, 1e4786_xdp, 1e4787_xdp, 1e4788_xdp, 1e4789_xdp, &
        1e4790_xdp, 1e4791_xdp, 1e4792_xdp, 1e4793_xdp, 1e4794_xdp, &
        1e4795_xdp, 1e4796_xdp, 1e4797_xdp, 1e4798_xdp, 1e4799_xdp, &
        1e4800_xdp, 1e4801_xdp, 1e4802_xdp, 1e4803_xdp, 1e4804_xdp, &
        1e4805_xdp, 1e4806_xdp, 1e4807_xdp, 1e4808_xdp, 1e4809_xdp, &
        1e4810_xdp, 1e4811_xdp, 1e4812_xdp, 1e4813_xdp, 1e4814_xdp, &
        1e4815_xdp, 1e4816_xdp, 1e4817_xdp, 1e4818_xdp, 1e4819_xdp, &
        1e4820_xdp, 1e4821_xdp, 1e4822_xdp, 1e4823_xdp, 1e4824_xdp, &
        1e4825_xdp, 1e4826_xdp, 1e4827_xdp, 1e4828_xdp, 1e4829_xdp, &
        1e4830_xdp, 1e4831_xdp, 1e4832_xdp, 1e4833_xdp, 1e4834_xdp, &
        1e4835_xdp, 1e4836_xdp, 1e4837_xdp, 1e4838_xdp, 1e4839_xdp, &
        1e4840_xdp, 1e4841_xdp, 1e4842_xdp, 1e4843_xdp, 1e4844_xdp, &
        1e4845_xdp, 1e4846_xdp, 1e4847_xdp, 1e4848_xdp, 1e4849_xdp, &
        1e4850_xdp, 1e4851_xdp, 1e4852_xdp, 1e4853_xdp, 1e4854_xdp, &
        1e4855_xdp, 1e4856_xdp, 1e4857_xdp, 1e4858_xdp, 1e4859_xdp, &
        1e4860_xdp, 1e4861_xdp, 1e4862_xdp, 1e4863_xdp, 1e4864_xdp, &
        1e4865_xdp, 1e4866_xdp, 1e4867_xdp, 1e4868_xdp, 1e4869_xdp, &
        1e4870_xdp, 1e4871_xdp, 1e4872_xdp, 1e4873_xdp, 1e4874_xdp, &
        1e4875_xdp, 1e4876_xdp, 1e4877_xdp, 1e4878_xdp, 1e4879_xdp, &
        1e4880_xdp, 1e4881_xdp, 1e4882_xdp, 1e4883_xdp, 1e4884_xdp, &
        1e4885_xdp, 1e4886_xdp, 1e4887_xdp, 1e4888_xdp, 1e4889_xdp, &
        1e4890_xdp, 1e4891_xdp, 1e4892_xdp, 1e4893_xdp, 1e4894_xdp, &
        1e4895_xdp, 1e4896_xdp, 1e4897_xdp, 1e4898_xdp, 1e4899_xdp, &
        1e4900_xdp, 1e4901_xdp, 1e4902_xdp, 1e4903_xdp, 1e4904_xdp, &
        1e4905_xdp, 1e4906_xdp, 1e4907_xdp, 1e4908_xdp, 1e4909_xdp, &
        1e4910_xdp, 1e4911_xdp, 1e4912_xdp, 1e4913_xdp, 1e4914_xdp, &
        1e4915_xdp, 1e4916_xdp, 1e4917_xdp, 1e4918_xdp, 1e4919_xdp, &
        1e4920_xdp, 1e4921_xdp, 1e4922_xdp, 1e4923_xdp, 1e4924_xdp, &
        1e4925_xdp, 1e4926_xdp, 1e4927_xdp, 1e4928_xdp, 1e4929_xdp, &
        1e4930_xdp, 1e4931_xdp, 1e4932_xdp, &
        REAL_XDP_INF &
    ]
    real(XDP), parameter :: REAL_XDP_DEC_POWERS(-4952:4933) = [ &
        REAL_XDP_DEC_POWERS1, &
        REAL_XDP_DEC_POWERS2, &
        REAL_XDP_DEC_POWERS3, &
        REAL_XDP_DEC_POWERS4, &
        REAL_XDP_DEC_POWERS5, &
        REAL_XDP_DEC_POWERS6, &
        REAL_XDP_DEC_POWERS7, &
        REAL_XDP_DEC_POWERS8 &
    ]
#endif
#ifdef _QP
    real(QP), parameter, public :: &
        REAL_QP_INF = _REAL_QP_INF, &
        REAL_QP_NAN = _REAL_QP_NAN
    real(QP), parameter :: REAL_QP_DEC_POWERS1(*) = [ &
        0._qp, tiny(0._qp) * epsilon(0._qp), &
        1e-4965_qp, 1e-4964_qp, 1e-4963_qp, 1e-4962_qp, 1e-4961_qp, &
        1e-4960_qp, 1e-4959_qp, 1e-4958_qp, 1e-4957_qp, 1e-4956_qp, &
        1e-4955_qp, 1e-4954_qp, 1e-4953_qp, 1e-4952_qp, 1e-4951_qp, &
        1e-4950_qp, 1e-4949_qp, 1e-4948_qp, 1e-4947_qp, 1e-4946_qp, &
        1e-4945_qp, 1e-4944_qp, 1e-4943_qp, 1e-4942_qp, 1e-4941_qp, &
        1e-4940_qp, 1e-4939_qp, 1e-4938_qp, 1e-4937_qp, 1e-4936_qp, &
        1e-4935_qp, 1e-4934_qp, 1e-4933_qp, 1e-4932_qp, 1e-4931_qp, &
        1e-4930_qp, 1e-4929_qp, 1e-4928_qp, 1e-4927_qp, 1e-4926_qp, &
        1e-4925_qp, 1e-4924_qp, 1e-4923_qp, 1e-4922_qp, 1e-4921_qp, &
        1e-4920_qp, 1e-4919_qp, 1e-4918_qp, 1e-4917_qp, 1e-4916_qp, &
        1e-4915_qp, 1e-4914_qp, 1e-4913_qp, 1e-4912_qp, 1e-4911_qp, &
        1e-4910_qp, 1e-4909_qp, 1e-4908_qp, 1e-4907_qp, 1e-4906_qp, &
        1e-4905_qp, 1e-4904_qp, 1e-4903_qp, 1e-4902_qp, 1e-4901_qp, &
        1e-4900_qp, 1e-4899_qp, 1e-4898_qp, 1e-4897_qp, 1e-4896_qp, &
        1e-4895_qp, 1e-4894_qp, 1e-4893_qp, 1e-4892_qp, 1e-4891_qp, &
        1e-4890_qp, 1e-4889_qp, 1e-4888_qp, 1e-4887_qp, 1e-4886_qp, &
        1e-4885_qp, 1e-4884_qp, 1e-4883_qp, 1e-4882_qp, 1e-4881_qp, &
        1e-4880_qp, 1e-4879_qp, 1e-4878_qp, 1e-4877_qp, 1e-4876_qp, &
        1e-4875_qp, 1e-4874_qp, 1e-4873_qp, 1e-4872_qp, 1e-4871_qp, &
        1e-4870_qp, 1e-4869_qp, 1e-4868_qp, 1e-4867_qp, 1e-4866_qp, &
        1e-4865_qp, 1e-4864_qp, 1e-4863_qp, 1e-4862_qp, 1e-4861_qp, &
        1e-4860_qp, 1e-4859_qp, 1e-4858_qp, 1e-4857_qp, 1e-4856_qp, &
        1e-4855_qp, 1e-4854_qp, 1e-4853_qp, 1e-4852_qp, 1e-4851_qp, &
        1e-4850_qp, 1e-4849_qp, 1e-4848_qp, 1e-4847_qp, 1e-4846_qp, &
        1e-4845_qp, 1e-4844_qp, 1e-4843_qp, 1e-4842_qp, 1e-4841_qp, &
        1e-4840_qp, 1e-4839_qp, 1e-4838_qp, 1e-4837_qp, 1e-4836_qp, &
        1e-4835_qp, 1e-4834_qp, 1e-4833_qp, 1e-4832_qp, 1e-4831_qp, &
        1e-4830_qp, 1e-4829_qp, 1e-4828_qp, 1e-4827_qp, 1e-4826_qp, &
        1e-4825_qp, 1e-4824_qp, 1e-4823_qp, 1e-4822_qp, 1e-4821_qp, &
        1e-4820_qp, 1e-4819_qp, 1e-4818_qp, 1e-4817_qp, 1e-4816_qp, &
        1e-4815_qp, 1e-4814_qp, 1e-4813_qp, 1e-4812_qp, 1e-4811_qp, &
        1e-4810_qp, 1e-4809_qp, 1e-4808_qp, 1e-4807_qp, 1e-4806_qp, &
        1e-4805_qp, 1e-4804_qp, 1e-4803_qp, 1e-4802_qp, 1e-4801_qp, &
        1e-4800_qp, 1e-4799_qp, 1e-4798_qp, 1e-4797_qp, 1e-4796_qp, &
        1e-4795_qp, 1e-4794_qp, 1e-4793_qp, 1e-4792_qp, 1e-4791_qp, &
        1e-4790_qp, 1e-4789_qp, 1e-4788_qp, 1e-4787_qp, 1e-4786_qp, &
        1e-4785_qp, 1e-4784_qp, 1e-4783_qp, 1e-4782_qp, 1e-4781_qp, &
        1e-4780_qp, 1e-4779_qp, 1e-4778_qp, 1e-4777_qp, 1e-4776_qp, &
        1e-4775_qp, 1e-4774_qp, 1e-4773_qp, 1e-4772_qp, 1e-4771_qp, &
        1e-4770_qp, 1e-4769_qp, 1e-4768_qp, 1e-4767_qp, 1e-4766_qp, &
        1e-4765_qp, 1e-4764_qp, 1e-4763_qp, 1e-4762_qp, 1e-4761_qp, &
        1e-4760_qp, 1e-4759_qp, 1e-4758_qp, 1e-4757_qp, 1e-4756_qp, &
        1e-4755_qp, 1e-4754_qp, 1e-4753_qp, 1e-4752_qp, 1e-4751_qp, &
        1e-4750_qp, 1e-4749_qp, 1e-4748_qp, 1e-4747_qp, 1e-4746_qp, &
        1e-4745_qp, 1e-4744_qp, 1e-4743_qp, 1e-4742_qp, 1e-4741_qp, &
        1e-4740_qp, 1e-4739_qp, 1e-4738_qp, 1e-4737_qp, 1e-4736_qp, &
        1e-4735_qp, 1e-4734_qp, 1e-4733_qp, 1e-4732_qp, 1e-4731_qp, &
        1e-4730_qp, 1e-4729_qp, 1e-4728_qp, 1e-4727_qp, 1e-4726_qp, &
        1e-4725_qp, 1e-4724_qp, 1e-4723_qp, 1e-4722_qp, 1e-4721_qp, &
        1e-4720_qp, 1e-4719_qp, 1e-4718_qp, 1e-4717_qp, 1e-4716_qp, &
        1e-4715_qp, 1e-4714_qp, 1e-4713_qp, 1e-4712_qp, 1e-4711_qp, &
        1e-4710_qp, 1e-4709_qp, 1e-4708_qp, 1e-4707_qp, 1e-4706_qp, &
        1e-4705_qp, 1e-4704_qp, 1e-4703_qp, 1e-4702_qp, 1e-4701_qp, &
        1e-4700_qp, 1e-4699_qp, 1e-4698_qp, 1e-4697_qp, 1e-4696_qp, &
        1e-4695_qp, 1e-4694_qp, 1e-4693_qp, 1e-4692_qp, 1e-4691_qp, &
        1e-4690_qp, 1e-4689_qp, 1e-4688_qp, 1e-4687_qp, 1e-4686_qp, &
        1e-4685_qp, 1e-4684_qp, 1e-4683_qp, 1e-4682_qp, 1e-4681_qp, &
        1e-4680_qp, 1e-4679_qp, 1e-4678_qp, 1e-4677_qp, 1e-4676_qp, &
        1e-4675_qp, 1e-4674_qp, 1e-4673_qp, 1e-4672_qp, 1e-4671_qp, &
        1e-4670_qp, 1e-4669_qp, 1e-4668_qp, 1e-4667_qp, 1e-4666_qp, &
        1e-4665_qp, 1e-4664_qp, 1e-4663_qp, 1e-4662_qp, 1e-4661_qp, &
        1e-4660_qp, 1e-4659_qp, 1e-4658_qp, 1e-4657_qp, 1e-4656_qp, &
        1e-4655_qp, 1e-4654_qp, 1e-4653_qp, 1e-4652_qp, 1e-4651_qp, &
        1e-4650_qp, 1e-4649_qp, 1e-4648_qp, 1e-4647_qp, 1e-4646_qp, &
        1e-4645_qp, 1e-4644_qp, 1e-4643_qp, 1e-4642_qp, 1e-4641_qp, &
        1e-4640_qp, 1e-4639_qp, 1e-4638_qp, 1e-4637_qp, 1e-4636_qp, &
        1e-4635_qp, 1e-4634_qp, 1e-4633_qp, 1e-4632_qp, 1e-4631_qp, &
        1e-4630_qp, 1e-4629_qp, 1e-4628_qp, 1e-4627_qp, 1e-4626_qp, &
        1e-4625_qp, 1e-4624_qp, 1e-4623_qp, 1e-4622_qp, 1e-4621_qp, &
        1e-4620_qp, 1e-4619_qp, 1e-4618_qp, 1e-4617_qp, 1e-4616_qp, &
        1e-4615_qp, 1e-4614_qp, 1e-4613_qp, 1e-4612_qp, 1e-4611_qp, &
        1e-4610_qp, 1e-4609_qp, 1e-4608_qp, 1e-4607_qp, 1e-4606_qp, &
        1e-4605_qp, 1e-4604_qp, 1e-4603_qp, 1e-4602_qp, 1e-4601_qp, &
        1e-4600_qp, 1e-4599_qp, 1e-4598_qp, 1e-4597_qp, 1e-4596_qp, &
        1e-4595_qp, 1e-4594_qp, 1e-4593_qp, 1e-4592_qp, 1e-4591_qp, &
        1e-4590_qp, 1e-4589_qp, 1e-4588_qp, 1e-4587_qp, 1e-4586_qp, &
        1e-4585_qp, 1e-4584_qp, 1e-4583_qp, 1e-4582_qp, 1e-4581_qp, &
        1e-4580_qp, 1e-4579_qp, 1e-4578_qp, 1e-4577_qp, 1e-4576_qp, &
        1e-4575_qp, 1e-4574_qp, 1e-4573_qp, 1e-4572_qp, 1e-4571_qp, &
        1e-4570_qp, 1e-4569_qp, 1e-4568_qp, 1e-4567_qp, 1e-4566_qp, &
        1e-4565_qp, 1e-4564_qp, 1e-4563_qp, 1e-4562_qp, 1e-4561_qp, &
        1e-4560_qp, 1e-4559_qp, 1e-4558_qp, 1e-4557_qp, 1e-4556_qp, &
        1e-4555_qp, 1e-4554_qp, 1e-4553_qp, 1e-4552_qp, 1e-4551_qp, &
        1e-4550_qp, 1e-4549_qp, 1e-4548_qp, 1e-4547_qp, 1e-4546_qp, &
        1e-4545_qp, 1e-4544_qp, 1e-4543_qp, 1e-4542_qp, 1e-4541_qp, &
        1e-4540_qp, 1e-4539_qp, 1e-4538_qp, 1e-4537_qp, 1e-4536_qp, &
        1e-4535_qp, 1e-4534_qp, 1e-4533_qp, 1e-4532_qp, 1e-4531_qp, &
        1e-4530_qp, 1e-4529_qp, 1e-4528_qp, 1e-4527_qp, 1e-4526_qp, &
        1e-4525_qp, 1e-4524_qp, 1e-4523_qp, 1e-4522_qp, 1e-4521_qp, &
        1e-4520_qp, 1e-4519_qp, 1e-4518_qp, 1e-4517_qp, 1e-4516_qp, &
        1e-4515_qp, 1e-4514_qp, 1e-4513_qp, 1e-4512_qp, 1e-4511_qp, &
        1e-4510_qp, 1e-4509_qp, 1e-4508_qp, 1e-4507_qp, 1e-4506_qp, &
        1e-4505_qp, 1e-4504_qp, 1e-4503_qp, 1e-4502_qp, 1e-4501_qp, &
        1e-4500_qp, 1e-4499_qp, 1e-4498_qp, 1e-4497_qp, 1e-4496_qp, &
        1e-4495_qp, 1e-4494_qp, 1e-4493_qp, 1e-4492_qp, 1e-4491_qp, &
        1e-4490_qp, 1e-4489_qp, 1e-4488_qp, 1e-4487_qp, 1e-4486_qp, &
        1e-4485_qp, 1e-4484_qp, 1e-4483_qp, 1e-4482_qp, 1e-4481_qp, &
        1e-4480_qp, 1e-4479_qp, 1e-4478_qp, 1e-4477_qp, 1e-4476_qp, &
        1e-4475_qp, 1e-4474_qp, 1e-4473_qp, 1e-4472_qp, 1e-4471_qp, &
        1e-4470_qp, 1e-4469_qp, 1e-4468_qp, 1e-4467_qp, 1e-4466_qp, &
        1e-4465_qp, 1e-4464_qp, 1e-4463_qp, 1e-4462_qp, 1e-4461_qp, &
        1e-4460_qp, 1e-4459_qp, 1e-4458_qp, 1e-4457_qp, 1e-4456_qp, &
        1e-4455_qp, 1e-4454_qp, 1e-4453_qp, 1e-4452_qp, 1e-4451_qp, &
        1e-4450_qp, 1e-4449_qp, 1e-4448_qp, 1e-4447_qp, 1e-4446_qp, &
        1e-4445_qp, 1e-4444_qp, 1e-4443_qp, 1e-4442_qp, 1e-4441_qp, &
        1e-4440_qp, 1e-4439_qp, 1e-4438_qp, 1e-4437_qp, 1e-4436_qp, &
        1e-4435_qp, 1e-4434_qp, 1e-4433_qp, 1e-4432_qp, 1e-4431_qp, &
        1e-4430_qp, 1e-4429_qp, 1e-4428_qp, 1e-4427_qp, 1e-4426_qp, &
        1e-4425_qp, 1e-4424_qp, 1e-4423_qp, 1e-4422_qp, 1e-4421_qp, &
        1e-4420_qp, 1e-4419_qp, 1e-4418_qp, 1e-4417_qp, 1e-4416_qp, &
        1e-4415_qp, 1e-4414_qp, 1e-4413_qp, 1e-4412_qp, 1e-4411_qp, &
        1e-4410_qp, 1e-4409_qp, 1e-4408_qp, 1e-4407_qp, 1e-4406_qp, &
        1e-4405_qp, 1e-4404_qp, 1e-4403_qp, 1e-4402_qp, 1e-4401_qp, &
        1e-4400_qp, 1e-4399_qp, 1e-4398_qp, 1e-4397_qp, 1e-4396_qp, &
        1e-4395_qp, 1e-4394_qp, 1e-4393_qp, 1e-4392_qp, 1e-4391_qp, &
        1e-4390_qp, 1e-4389_qp, 1e-4388_qp, 1e-4387_qp, 1e-4386_qp, &
        1e-4385_qp, 1e-4384_qp, 1e-4383_qp, 1e-4382_qp, 1e-4381_qp, &
        1e-4380_qp, 1e-4379_qp, 1e-4378_qp, 1e-4377_qp, 1e-4376_qp, &
        1e-4375_qp, 1e-4374_qp, 1e-4373_qp, 1e-4372_qp, 1e-4371_qp, &
        1e-4370_qp, 1e-4369_qp, 1e-4368_qp, 1e-4367_qp, 1e-4366_qp, &
        1e-4365_qp, 1e-4364_qp, 1e-4363_qp, 1e-4362_qp, 1e-4361_qp, &
        1e-4360_qp, 1e-4359_qp, 1e-4358_qp, 1e-4357_qp, 1e-4356_qp, &
        1e-4355_qp, 1e-4354_qp, 1e-4353_qp, 1e-4352_qp, 1e-4351_qp, &
        1e-4350_qp, 1e-4349_qp, 1e-4348_qp, 1e-4347_qp, 1e-4346_qp, &
        1e-4345_qp, 1e-4344_qp, 1e-4343_qp, 1e-4342_qp, 1e-4341_qp, &
        1e-4340_qp, 1e-4339_qp, 1e-4338_qp, 1e-4337_qp, 1e-4336_qp, &
        1e-4335_qp, 1e-4334_qp, 1e-4333_qp, 1e-4332_qp, 1e-4331_qp, &
        1e-4330_qp, 1e-4329_qp, 1e-4328_qp, 1e-4327_qp, 1e-4326_qp, &
        1e-4325_qp, 1e-4324_qp, 1e-4323_qp, 1e-4322_qp, 1e-4321_qp, &
        1e-4320_qp, 1e-4319_qp, 1e-4318_qp, 1e-4317_qp, 1e-4316_qp, &
        1e-4315_qp, 1e-4314_qp, 1e-4313_qp, 1e-4312_qp, 1e-4311_qp, &
        1e-4310_qp, 1e-4309_qp, 1e-4308_qp, 1e-4307_qp, 1e-4306_qp, &
        1e-4305_qp, 1e-4304_qp, 1e-4303_qp, 1e-4302_qp, 1e-4301_qp, &
        1e-4300_qp, 1e-4299_qp, 1e-4298_qp, 1e-4297_qp, 1e-4296_qp, &
        1e-4295_qp, 1e-4294_qp, 1e-4293_qp, 1e-4292_qp, 1e-4291_qp, &
        1e-4290_qp, 1e-4289_qp, 1e-4288_qp, 1e-4287_qp, 1e-4286_qp, &
        1e-4285_qp, 1e-4284_qp, 1e-4283_qp, 1e-4282_qp, 1e-4281_qp, &
        1e-4280_qp, 1e-4279_qp, 1e-4278_qp, 1e-4277_qp, 1e-4276_qp, &
        1e-4275_qp, 1e-4274_qp, 1e-4273_qp, 1e-4272_qp, 1e-4271_qp, &
        1e-4270_qp, 1e-4269_qp, 1e-4268_qp, 1e-4267_qp, 1e-4266_qp, &
        1e-4265_qp, 1e-4264_qp, 1e-4263_qp, 1e-4262_qp, 1e-4261_qp, &
        1e-4260_qp, 1e-4259_qp, 1e-4258_qp, 1e-4257_qp, 1e-4256_qp, &
        1e-4255_qp, 1e-4254_qp, 1e-4253_qp, 1e-4252_qp, 1e-4251_qp, &
        1e-4250_qp, 1e-4249_qp, 1e-4248_qp, 1e-4247_qp, 1e-4246_qp, &
        1e-4245_qp, 1e-4244_qp, 1e-4243_qp, 1e-4242_qp, 1e-4241_qp, &
        1e-4240_qp, 1e-4239_qp, 1e-4238_qp, 1e-4237_qp, 1e-4236_qp, &
        1e-4235_qp, 1e-4234_qp, 1e-4233_qp, 1e-4232_qp, 1e-4231_qp, &
        1e-4230_qp, 1e-4229_qp, 1e-4228_qp, 1e-4227_qp, 1e-4226_qp, &
        1e-4225_qp, 1e-4224_qp, 1e-4223_qp, 1e-4222_qp, 1e-4221_qp, &
        1e-4220_qp, 1e-4219_qp, 1e-4218_qp, 1e-4217_qp, 1e-4216_qp, &
        1e-4215_qp, 1e-4214_qp, 1e-4213_qp, 1e-4212_qp, 1e-4211_qp, &
        1e-4210_qp, 1e-4209_qp, 1e-4208_qp, 1e-4207_qp, 1e-4206_qp, &
        1e-4205_qp, 1e-4204_qp, 1e-4203_qp, 1e-4202_qp, 1e-4201_qp, &
        1e-4200_qp, 1e-4199_qp, 1e-4198_qp, 1e-4197_qp, 1e-4196_qp, &
        1e-4195_qp, 1e-4194_qp, 1e-4193_qp, 1e-4192_qp, 1e-4191_qp, &
        1e-4190_qp, 1e-4189_qp, 1e-4188_qp, 1e-4187_qp, 1e-4186_qp, &
        1e-4185_qp, 1e-4184_qp, 1e-4183_qp, 1e-4182_qp, 1e-4181_qp, &
        1e-4180_qp, 1e-4179_qp, 1e-4178_qp, 1e-4177_qp, 1e-4176_qp, &
        1e-4175_qp, 1e-4174_qp, 1e-4173_qp, 1e-4172_qp, 1e-4171_qp, &
        1e-4170_qp, 1e-4169_qp, 1e-4168_qp, 1e-4167_qp, 1e-4166_qp, &
        1e-4165_qp, 1e-4164_qp, 1e-4163_qp, 1e-4162_qp, 1e-4161_qp, &
        1e-4160_qp, 1e-4159_qp, 1e-4158_qp, 1e-4157_qp, 1e-4156_qp, &
        1e-4155_qp, 1e-4154_qp, 1e-4153_qp, 1e-4152_qp, 1e-4151_qp, &
        1e-4150_qp, 1e-4149_qp, 1e-4148_qp, 1e-4147_qp, 1e-4146_qp, &
        1e-4145_qp, 1e-4144_qp, 1e-4143_qp, 1e-4142_qp, 1e-4141_qp, &
        1e-4140_qp, 1e-4139_qp, 1e-4138_qp, 1e-4137_qp, 1e-4136_qp, &
        1e-4135_qp, 1e-4134_qp, 1e-4133_qp, 1e-4132_qp, 1e-4131_qp, &
        1e-4130_qp, 1e-4129_qp, 1e-4128_qp, 1e-4127_qp, 1e-4126_qp, &
        1e-4125_qp, 1e-4124_qp, 1e-4123_qp, 1e-4122_qp, 1e-4121_qp, &
        1e-4120_qp, 1e-4119_qp, 1e-4118_qp, 1e-4117_qp, 1e-4116_qp, &
        1e-4115_qp, 1e-4114_qp, 1e-4113_qp, 1e-4112_qp, 1e-4111_qp, &
        1e-4110_qp, 1e-4109_qp, 1e-4108_qp, 1e-4107_qp, 1e-4106_qp, &
        1e-4105_qp, 1e-4104_qp, 1e-4103_qp, 1e-4102_qp, 1e-4101_qp, &
        1e-4100_qp, 1e-4099_qp, 1e-4098_qp, 1e-4097_qp, 1e-4096_qp, &
        1e-4095_qp, 1e-4094_qp, 1e-4093_qp, 1e-4092_qp, 1e-4091_qp, &
        1e-4090_qp, 1e-4089_qp, 1e-4088_qp, 1e-4087_qp, 1e-4086_qp, &
        1e-4085_qp, 1e-4084_qp, 1e-4083_qp, 1e-4082_qp, 1e-4081_qp, &
        1e-4080_qp, 1e-4079_qp, 1e-4078_qp, 1e-4077_qp, 1e-4076_qp, &
        1e-4075_qp, 1e-4074_qp, 1e-4073_qp, 1e-4072_qp, 1e-4071_qp, &
        1e-4070_qp, 1e-4069_qp, 1e-4068_qp, 1e-4067_qp, 1e-4066_qp, &
        1e-4065_qp, 1e-4064_qp, 1e-4063_qp, 1e-4062_qp, 1e-4061_qp, &
        1e-4060_qp, 1e-4059_qp, 1e-4058_qp, 1e-4057_qp, 1e-4056_qp, &
        1e-4055_qp, 1e-4054_qp, 1e-4053_qp, 1e-4052_qp, 1e-4051_qp, &
        1e-4050_qp, 1e-4049_qp, 1e-4048_qp, 1e-4047_qp, 1e-4046_qp, &
        1e-4045_qp, 1e-4044_qp, 1e-4043_qp, 1e-4042_qp, 1e-4041_qp, &
        1e-4040_qp, 1e-4039_qp, 1e-4038_qp, 1e-4037_qp, 1e-4036_qp, &
        1e-4035_qp, 1e-4034_qp, 1e-4033_qp, 1e-4032_qp, 1e-4031_qp, &
        1e-4030_qp, 1e-4029_qp, 1e-4028_qp, 1e-4027_qp, 1e-4026_qp, &
        1e-4025_qp, 1e-4024_qp, 1e-4023_qp, 1e-4022_qp, 1e-4021_qp, &
        1e-4020_qp, 1e-4019_qp, 1e-4018_qp, 1e-4017_qp, 1e-4016_qp, &
        1e-4015_qp, 1e-4014_qp, 1e-4013_qp, 1e-4012_qp, 1e-4011_qp, &
        1e-4010_qp, 1e-4009_qp, 1e-4008_qp, 1e-4007_qp, 1e-4006_qp, &
        1e-4005_qp, 1e-4004_qp, 1e-4003_qp, 1e-4002_qp, 1e-4001_qp, &
        1e-4000_qp, 1e-3999_qp, 1e-3998_qp, 1e-3997_qp, 1e-3996_qp, &
        1e-3995_qp, 1e-3994_qp, 1e-3993_qp, 1e-3992_qp, 1e-3991_qp, &
        1e-3990_qp, 1e-3989_qp, 1e-3988_qp, 1e-3987_qp, 1e-3986_qp, &
        1e-3985_qp, 1e-3984_qp, 1e-3983_qp, 1e-3982_qp, 1e-3981_qp, &
        1e-3980_qp, 1e-3979_qp, 1e-3978_qp, 1e-3977_qp, 1e-3976_qp, &
        1e-3975_qp, 1e-3974_qp, 1e-3973_qp, 1e-3972_qp, 1e-3971_qp, &
        1e-3970_qp, 1e-3969_qp, 1e-3968_qp, 1e-3967_qp, 1e-3966_qp, &
        1e-3965_qp, 1e-3964_qp, 1e-3963_qp, 1e-3962_qp, 1e-3961_qp, &
        1e-3960_qp, 1e-3959_qp, 1e-3958_qp, 1e-3957_qp, 1e-3956_qp, &
        1e-3955_qp, 1e-3954_qp, 1e-3953_qp, 1e-3952_qp, 1e-3951_qp, &
        1e-3950_qp, 1e-3949_qp, 1e-3948_qp, 1e-3947_qp, 1e-3946_qp, &
        1e-3945_qp, 1e-3944_qp, 1e-3943_qp, 1e-3942_qp, 1e-3941_qp, &
        1e-3940_qp, 1e-3939_qp, 1e-3938_qp, 1e-3937_qp, 1e-3936_qp, &
        1e-3935_qp, 1e-3934_qp, 1e-3933_qp, 1e-3932_qp, 1e-3931_qp, &
        1e-3930_qp, 1e-3929_qp, 1e-3928_qp, 1e-3927_qp, 1e-3926_qp, &
        1e-3925_qp, 1e-3924_qp, 1e-3923_qp, 1e-3922_qp, 1e-3921_qp, &
        1e-3920_qp, 1e-3919_qp, 1e-3918_qp, 1e-3917_qp, 1e-3916_qp, &
        1e-3915_qp, 1e-3914_qp, 1e-3913_qp, 1e-3912_qp, 1e-3911_qp, &
        1e-3910_qp, 1e-3909_qp, 1e-3908_qp, 1e-3907_qp, 1e-3906_qp, &
        1e-3905_qp, 1e-3904_qp, 1e-3903_qp, 1e-3902_qp, 1e-3901_qp, &
        1e-3900_qp, 1e-3899_qp, 1e-3898_qp, 1e-3897_qp, 1e-3896_qp, &
        1e-3895_qp, 1e-3894_qp, 1e-3893_qp, 1e-3892_qp, 1e-3891_qp, &
        1e-3890_qp, 1e-3889_qp, 1e-3888_qp, 1e-3887_qp, 1e-3886_qp, &
        1e-3885_qp, 1e-3884_qp, 1e-3883_qp, 1e-3882_qp, 1e-3881_qp, &
        1e-3880_qp, 1e-3879_qp, 1e-3878_qp, 1e-3877_qp, 1e-3876_qp, &
        1e-3875_qp, 1e-3874_qp, 1e-3873_qp, 1e-3872_qp, 1e-3871_qp, &
        1e-3870_qp, 1e-3869_qp, 1e-3868_qp, 1e-3867_qp, 1e-3866_qp, &
        1e-3865_qp, 1e-3864_qp, 1e-3863_qp, 1e-3862_qp, 1e-3861_qp, &
        1e-3860_qp, 1e-3859_qp, 1e-3858_qp, 1e-3857_qp, 1e-3856_qp, &
        1e-3855_qp, 1e-3854_qp, 1e-3853_qp, 1e-3852_qp, 1e-3851_qp, &
        1e-3850_qp, 1e-3849_qp, 1e-3848_qp, 1e-3847_qp, 1e-3846_qp, &
        1e-3845_qp, 1e-3844_qp, 1e-3843_qp, 1e-3842_qp, 1e-3841_qp, &
        1e-3840_qp, 1e-3839_qp, 1e-3838_qp, 1e-3837_qp, 1e-3836_qp, &
        1e-3835_qp, 1e-3834_qp, 1e-3833_qp, 1e-3832_qp, 1e-3831_qp, &
        1e-3830_qp, 1e-3829_qp, 1e-3828_qp, 1e-3827_qp, 1e-3826_qp, &
        1e-3825_qp, 1e-3824_qp, 1e-3823_qp, 1e-3822_qp, 1e-3821_qp, &
        1e-3820_qp, 1e-3819_qp, 1e-3818_qp, 1e-3817_qp, 1e-3816_qp, &
        1e-3815_qp, 1e-3814_qp, 1e-3813_qp, 1e-3812_qp, 1e-3811_qp, &
        1e-3810_qp, 1e-3809_qp, 1e-3808_qp, 1e-3807_qp, 1e-3806_qp, &
        1e-3805_qp, 1e-3804_qp, 1e-3803_qp, 1e-3802_qp, 1e-3801_qp, &
        1e-3800_qp, 1e-3799_qp, 1e-3798_qp, 1e-3797_qp, 1e-3796_qp, &
        1e-3795_qp, 1e-3794_qp, 1e-3793_qp, 1e-3792_qp, 1e-3791_qp, &
        1e-3790_qp, 1e-3789_qp, 1e-3788_qp, 1e-3787_qp, 1e-3786_qp, &
        1e-3785_qp, 1e-3784_qp, 1e-3783_qp, 1e-3782_qp, 1e-3781_qp, &
        1e-3780_qp, 1e-3779_qp, 1e-3778_qp, 1e-3777_qp, 1e-3776_qp, &
        1e-3775_qp, 1e-3774_qp, 1e-3773_qp, 1e-3772_qp, 1e-3771_qp, &
        1e-3770_qp, 1e-3769_qp, 1e-3768_qp, 1e-3767_qp, 1e-3766_qp, &
        1e-3765_qp, 1e-3764_qp, 1e-3763_qp, 1e-3762_qp, 1e-3761_qp, &
        1e-3760_qp, 1e-3759_qp, 1e-3758_qp, 1e-3757_qp, 1e-3756_qp, &
        1e-3755_qp, 1e-3754_qp, 1e-3753_qp, 1e-3752_qp, 1e-3751_qp, &
        1e-3750_qp, 1e-3749_qp, 1e-3748_qp, 1e-3747_qp, 1e-3746_qp, &
        1e-3745_qp, 1e-3744_qp, 1e-3743_qp, 1e-3742_qp, 1e-3741_qp, &
        1e-3740_qp, 1e-3739_qp, 1e-3738_qp, 1e-3737_qp, 1e-3736_qp, &
        1e-3735_qp, 1e-3734_qp, 1e-3733_qp, 1e-3732_qp, 1e-3731_qp, &
        1e-3730_qp, 1e-3729_qp, 1e-3728_qp, 1e-3727_qp, 1e-3726_qp, &
        1e-3725_qp, 1e-3724_qp, 1e-3723_qp, 1e-3722_qp, 1e-3721_qp, &
        1e-3720_qp, 1e-3719_qp, 1e-3718_qp, 1e-3717_qp, 1e-3716_qp, &
        1e-3715_qp, 1e-3714_qp, 1e-3713_qp, 1e-3712_qp, 1e-3711_qp, &
        1e-3710_qp, 1e-3709_qp, 1e-3708_qp, 1e-3707_qp, 1e-3706_qp, &
        1e-3705_qp, 1e-3704_qp, 1e-3703_qp, 1e-3702_qp, 1e-3701_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS2(*) = [ &
        1e-3700_qp, 1e-3699_qp, 1e-3698_qp, 1e-3697_qp, 1e-3696_qp, &
        1e-3695_qp, 1e-3694_qp, 1e-3693_qp, 1e-3692_qp, 1e-3691_qp, &
        1e-3690_qp, 1e-3689_qp, 1e-3688_qp, 1e-3687_qp, 1e-3686_qp, &
        1e-3685_qp, 1e-3684_qp, 1e-3683_qp, 1e-3682_qp, 1e-3681_qp, &
        1e-3680_qp, 1e-3679_qp, 1e-3678_qp, 1e-3677_qp, 1e-3676_qp, &
        1e-3675_qp, 1e-3674_qp, 1e-3673_qp, 1e-3672_qp, 1e-3671_qp, &
        1e-3670_qp, 1e-3669_qp, 1e-3668_qp, 1e-3667_qp, 1e-3666_qp, &
        1e-3665_qp, 1e-3664_qp, 1e-3663_qp, 1e-3662_qp, 1e-3661_qp, &
        1e-3660_qp, 1e-3659_qp, 1e-3658_qp, 1e-3657_qp, 1e-3656_qp, &
        1e-3655_qp, 1e-3654_qp, 1e-3653_qp, 1e-3652_qp, 1e-3651_qp, &
        1e-3650_qp, 1e-3649_qp, 1e-3648_qp, 1e-3647_qp, 1e-3646_qp, &
        1e-3645_qp, 1e-3644_qp, 1e-3643_qp, 1e-3642_qp, 1e-3641_qp, &
        1e-3640_qp, 1e-3639_qp, 1e-3638_qp, 1e-3637_qp, 1e-3636_qp, &
        1e-3635_qp, 1e-3634_qp, 1e-3633_qp, 1e-3632_qp, 1e-3631_qp, &
        1e-3630_qp, 1e-3629_qp, 1e-3628_qp, 1e-3627_qp, 1e-3626_qp, &
        1e-3625_qp, 1e-3624_qp, 1e-3623_qp, 1e-3622_qp, 1e-3621_qp, &
        1e-3620_qp, 1e-3619_qp, 1e-3618_qp, 1e-3617_qp, 1e-3616_qp, &
        1e-3615_qp, 1e-3614_qp, 1e-3613_qp, 1e-3612_qp, 1e-3611_qp, &
        1e-3610_qp, 1e-3609_qp, 1e-3608_qp, 1e-3607_qp, 1e-3606_qp, &
        1e-3605_qp, 1e-3604_qp, 1e-3603_qp, 1e-3602_qp, 1e-3601_qp, &
        1e-3600_qp, 1e-3599_qp, 1e-3598_qp, 1e-3597_qp, 1e-3596_qp, &
        1e-3595_qp, 1e-3594_qp, 1e-3593_qp, 1e-3592_qp, 1e-3591_qp, &
        1e-3590_qp, 1e-3589_qp, 1e-3588_qp, 1e-3587_qp, 1e-3586_qp, &
        1e-3585_qp, 1e-3584_qp, 1e-3583_qp, 1e-3582_qp, 1e-3581_qp, &
        1e-3580_qp, 1e-3579_qp, 1e-3578_qp, 1e-3577_qp, 1e-3576_qp, &
        1e-3575_qp, 1e-3574_qp, 1e-3573_qp, 1e-3572_qp, 1e-3571_qp, &
        1e-3570_qp, 1e-3569_qp, 1e-3568_qp, 1e-3567_qp, 1e-3566_qp, &
        1e-3565_qp, 1e-3564_qp, 1e-3563_qp, 1e-3562_qp, 1e-3561_qp, &
        1e-3560_qp, 1e-3559_qp, 1e-3558_qp, 1e-3557_qp, 1e-3556_qp, &
        1e-3555_qp, 1e-3554_qp, 1e-3553_qp, 1e-3552_qp, 1e-3551_qp, &
        1e-3550_qp, 1e-3549_qp, 1e-3548_qp, 1e-3547_qp, 1e-3546_qp, &
        1e-3545_qp, 1e-3544_qp, 1e-3543_qp, 1e-3542_qp, 1e-3541_qp, &
        1e-3540_qp, 1e-3539_qp, 1e-3538_qp, 1e-3537_qp, 1e-3536_qp, &
        1e-3535_qp, 1e-3534_qp, 1e-3533_qp, 1e-3532_qp, 1e-3531_qp, &
        1e-3530_qp, 1e-3529_qp, 1e-3528_qp, 1e-3527_qp, 1e-3526_qp, &
        1e-3525_qp, 1e-3524_qp, 1e-3523_qp, 1e-3522_qp, 1e-3521_qp, &
        1e-3520_qp, 1e-3519_qp, 1e-3518_qp, 1e-3517_qp, 1e-3516_qp, &
        1e-3515_qp, 1e-3514_qp, 1e-3513_qp, 1e-3512_qp, 1e-3511_qp, &
        1e-3510_qp, 1e-3509_qp, 1e-3508_qp, 1e-3507_qp, 1e-3506_qp, &
        1e-3505_qp, 1e-3504_qp, 1e-3503_qp, 1e-3502_qp, 1e-3501_qp, &
        1e-3500_qp, 1e-3499_qp, 1e-3498_qp, 1e-3497_qp, 1e-3496_qp, &
        1e-3495_qp, 1e-3494_qp, 1e-3493_qp, 1e-3492_qp, 1e-3491_qp, &
        1e-3490_qp, 1e-3489_qp, 1e-3488_qp, 1e-3487_qp, 1e-3486_qp, &
        1e-3485_qp, 1e-3484_qp, 1e-3483_qp, 1e-3482_qp, 1e-3481_qp, &
        1e-3480_qp, 1e-3479_qp, 1e-3478_qp, 1e-3477_qp, 1e-3476_qp, &
        1e-3475_qp, 1e-3474_qp, 1e-3473_qp, 1e-3472_qp, 1e-3471_qp, &
        1e-3470_qp, 1e-3469_qp, 1e-3468_qp, 1e-3467_qp, 1e-3466_qp, &
        1e-3465_qp, 1e-3464_qp, 1e-3463_qp, 1e-3462_qp, 1e-3461_qp, &
        1e-3460_qp, 1e-3459_qp, 1e-3458_qp, 1e-3457_qp, 1e-3456_qp, &
        1e-3455_qp, 1e-3454_qp, 1e-3453_qp, 1e-3452_qp, 1e-3451_qp, &
        1e-3450_qp, 1e-3449_qp, 1e-3448_qp, 1e-3447_qp, 1e-3446_qp, &
        1e-3445_qp, 1e-3444_qp, 1e-3443_qp, 1e-3442_qp, 1e-3441_qp, &
        1e-3440_qp, 1e-3439_qp, 1e-3438_qp, 1e-3437_qp, 1e-3436_qp, &
        1e-3435_qp, 1e-3434_qp, 1e-3433_qp, 1e-3432_qp, 1e-3431_qp, &
        1e-3430_qp, 1e-3429_qp, 1e-3428_qp, 1e-3427_qp, 1e-3426_qp, &
        1e-3425_qp, 1e-3424_qp, 1e-3423_qp, 1e-3422_qp, 1e-3421_qp, &
        1e-3420_qp, 1e-3419_qp, 1e-3418_qp, 1e-3417_qp, 1e-3416_qp, &
        1e-3415_qp, 1e-3414_qp, 1e-3413_qp, 1e-3412_qp, 1e-3411_qp, &
        1e-3410_qp, 1e-3409_qp, 1e-3408_qp, 1e-3407_qp, 1e-3406_qp, &
        1e-3405_qp, 1e-3404_qp, 1e-3403_qp, 1e-3402_qp, 1e-3401_qp, &
        1e-3400_qp, 1e-3399_qp, 1e-3398_qp, 1e-3397_qp, 1e-3396_qp, &
        1e-3395_qp, 1e-3394_qp, 1e-3393_qp, 1e-3392_qp, 1e-3391_qp, &
        1e-3390_qp, 1e-3389_qp, 1e-3388_qp, 1e-3387_qp, 1e-3386_qp, &
        1e-3385_qp, 1e-3384_qp, 1e-3383_qp, 1e-3382_qp, 1e-3381_qp, &
        1e-3380_qp, 1e-3379_qp, 1e-3378_qp, 1e-3377_qp, 1e-3376_qp, &
        1e-3375_qp, 1e-3374_qp, 1e-3373_qp, 1e-3372_qp, 1e-3371_qp, &
        1e-3370_qp, 1e-3369_qp, 1e-3368_qp, 1e-3367_qp, 1e-3366_qp, &
        1e-3365_qp, 1e-3364_qp, 1e-3363_qp, 1e-3362_qp, 1e-3361_qp, &
        1e-3360_qp, 1e-3359_qp, 1e-3358_qp, 1e-3357_qp, 1e-3356_qp, &
        1e-3355_qp, 1e-3354_qp, 1e-3353_qp, 1e-3352_qp, 1e-3351_qp, &
        1e-3350_qp, 1e-3349_qp, 1e-3348_qp, 1e-3347_qp, 1e-3346_qp, &
        1e-3345_qp, 1e-3344_qp, 1e-3343_qp, 1e-3342_qp, 1e-3341_qp, &
        1e-3340_qp, 1e-3339_qp, 1e-3338_qp, 1e-3337_qp, 1e-3336_qp, &
        1e-3335_qp, 1e-3334_qp, 1e-3333_qp, 1e-3332_qp, 1e-3331_qp, &
        1e-3330_qp, 1e-3329_qp, 1e-3328_qp, 1e-3327_qp, 1e-3326_qp, &
        1e-3325_qp, 1e-3324_qp, 1e-3323_qp, 1e-3322_qp, 1e-3321_qp, &
        1e-3320_qp, 1e-3319_qp, 1e-3318_qp, 1e-3317_qp, 1e-3316_qp, &
        1e-3315_qp, 1e-3314_qp, 1e-3313_qp, 1e-3312_qp, 1e-3311_qp, &
        1e-3310_qp, 1e-3309_qp, 1e-3308_qp, 1e-3307_qp, 1e-3306_qp, &
        1e-3305_qp, 1e-3304_qp, 1e-3303_qp, 1e-3302_qp, 1e-3301_qp, &
        1e-3300_qp, 1e-3299_qp, 1e-3298_qp, 1e-3297_qp, 1e-3296_qp, &
        1e-3295_qp, 1e-3294_qp, 1e-3293_qp, 1e-3292_qp, 1e-3291_qp, &
        1e-3290_qp, 1e-3289_qp, 1e-3288_qp, 1e-3287_qp, 1e-3286_qp, &
        1e-3285_qp, 1e-3284_qp, 1e-3283_qp, 1e-3282_qp, 1e-3281_qp, &
        1e-3280_qp, 1e-3279_qp, 1e-3278_qp, 1e-3277_qp, 1e-3276_qp, &
        1e-3275_qp, 1e-3274_qp, 1e-3273_qp, 1e-3272_qp, 1e-3271_qp, &
        1e-3270_qp, 1e-3269_qp, 1e-3268_qp, 1e-3267_qp, 1e-3266_qp, &
        1e-3265_qp, 1e-3264_qp, 1e-3263_qp, 1e-3262_qp, 1e-3261_qp, &
        1e-3260_qp, 1e-3259_qp, 1e-3258_qp, 1e-3257_qp, 1e-3256_qp, &
        1e-3255_qp, 1e-3254_qp, 1e-3253_qp, 1e-3252_qp, 1e-3251_qp, &
        1e-3250_qp, 1e-3249_qp, 1e-3248_qp, 1e-3247_qp, 1e-3246_qp, &
        1e-3245_qp, 1e-3244_qp, 1e-3243_qp, 1e-3242_qp, 1e-3241_qp, &
        1e-3240_qp, 1e-3239_qp, 1e-3238_qp, 1e-3237_qp, 1e-3236_qp, &
        1e-3235_qp, 1e-3234_qp, 1e-3233_qp, 1e-3232_qp, 1e-3231_qp, &
        1e-3230_qp, 1e-3229_qp, 1e-3228_qp, 1e-3227_qp, 1e-3226_qp, &
        1e-3225_qp, 1e-3224_qp, 1e-3223_qp, 1e-3222_qp, 1e-3221_qp, &
        1e-3220_qp, 1e-3219_qp, 1e-3218_qp, 1e-3217_qp, 1e-3216_qp, &
        1e-3215_qp, 1e-3214_qp, 1e-3213_qp, 1e-3212_qp, 1e-3211_qp, &
        1e-3210_qp, 1e-3209_qp, 1e-3208_qp, 1e-3207_qp, 1e-3206_qp, &
        1e-3205_qp, 1e-3204_qp, 1e-3203_qp, 1e-3202_qp, 1e-3201_qp, &
        1e-3200_qp, 1e-3199_qp, 1e-3198_qp, 1e-3197_qp, 1e-3196_qp, &
        1e-3195_qp, 1e-3194_qp, 1e-3193_qp, 1e-3192_qp, 1e-3191_qp, &
        1e-3190_qp, 1e-3189_qp, 1e-3188_qp, 1e-3187_qp, 1e-3186_qp, &
        1e-3185_qp, 1e-3184_qp, 1e-3183_qp, 1e-3182_qp, 1e-3181_qp, &
        1e-3180_qp, 1e-3179_qp, 1e-3178_qp, 1e-3177_qp, 1e-3176_qp, &
        1e-3175_qp, 1e-3174_qp, 1e-3173_qp, 1e-3172_qp, 1e-3171_qp, &
        1e-3170_qp, 1e-3169_qp, 1e-3168_qp, 1e-3167_qp, 1e-3166_qp, &
        1e-3165_qp, 1e-3164_qp, 1e-3163_qp, 1e-3162_qp, 1e-3161_qp, &
        1e-3160_qp, 1e-3159_qp, 1e-3158_qp, 1e-3157_qp, 1e-3156_qp, &
        1e-3155_qp, 1e-3154_qp, 1e-3153_qp, 1e-3152_qp, 1e-3151_qp, &
        1e-3150_qp, 1e-3149_qp, 1e-3148_qp, 1e-3147_qp, 1e-3146_qp, &
        1e-3145_qp, 1e-3144_qp, 1e-3143_qp, 1e-3142_qp, 1e-3141_qp, &
        1e-3140_qp, 1e-3139_qp, 1e-3138_qp, 1e-3137_qp, 1e-3136_qp, &
        1e-3135_qp, 1e-3134_qp, 1e-3133_qp, 1e-3132_qp, 1e-3131_qp, &
        1e-3130_qp, 1e-3129_qp, 1e-3128_qp, 1e-3127_qp, 1e-3126_qp, &
        1e-3125_qp, 1e-3124_qp, 1e-3123_qp, 1e-3122_qp, 1e-3121_qp, &
        1e-3120_qp, 1e-3119_qp, 1e-3118_qp, 1e-3117_qp, 1e-3116_qp, &
        1e-3115_qp, 1e-3114_qp, 1e-3113_qp, 1e-3112_qp, 1e-3111_qp, &
        1e-3110_qp, 1e-3109_qp, 1e-3108_qp, 1e-3107_qp, 1e-3106_qp, &
        1e-3105_qp, 1e-3104_qp, 1e-3103_qp, 1e-3102_qp, 1e-3101_qp, &
        1e-3100_qp, 1e-3099_qp, 1e-3098_qp, 1e-3097_qp, 1e-3096_qp, &
        1e-3095_qp, 1e-3094_qp, 1e-3093_qp, 1e-3092_qp, 1e-3091_qp, &
        1e-3090_qp, 1e-3089_qp, 1e-3088_qp, 1e-3087_qp, 1e-3086_qp, &
        1e-3085_qp, 1e-3084_qp, 1e-3083_qp, 1e-3082_qp, 1e-3081_qp, &
        1e-3080_qp, 1e-3079_qp, 1e-3078_qp, 1e-3077_qp, 1e-3076_qp, &
        1e-3075_qp, 1e-3074_qp, 1e-3073_qp, 1e-3072_qp, 1e-3071_qp, &
        1e-3070_qp, 1e-3069_qp, 1e-3068_qp, 1e-3067_qp, 1e-3066_qp, &
        1e-3065_qp, 1e-3064_qp, 1e-3063_qp, 1e-3062_qp, 1e-3061_qp, &
        1e-3060_qp, 1e-3059_qp, 1e-3058_qp, 1e-3057_qp, 1e-3056_qp, &
        1e-3055_qp, 1e-3054_qp, 1e-3053_qp, 1e-3052_qp, 1e-3051_qp, &
        1e-3050_qp, 1e-3049_qp, 1e-3048_qp, 1e-3047_qp, 1e-3046_qp, &
        1e-3045_qp, 1e-3044_qp, 1e-3043_qp, 1e-3042_qp, 1e-3041_qp, &
        1e-3040_qp, 1e-3039_qp, 1e-3038_qp, 1e-3037_qp, 1e-3036_qp, &
        1e-3035_qp, 1e-3034_qp, 1e-3033_qp, 1e-3032_qp, 1e-3031_qp, &
        1e-3030_qp, 1e-3029_qp, 1e-3028_qp, 1e-3027_qp, 1e-3026_qp, &
        1e-3025_qp, 1e-3024_qp, 1e-3023_qp, 1e-3022_qp, 1e-3021_qp, &
        1e-3020_qp, 1e-3019_qp, 1e-3018_qp, 1e-3017_qp, 1e-3016_qp, &
        1e-3015_qp, 1e-3014_qp, 1e-3013_qp, 1e-3012_qp, 1e-3011_qp, &
        1e-3010_qp, 1e-3009_qp, 1e-3008_qp, 1e-3007_qp, 1e-3006_qp, &
        1e-3005_qp, 1e-3004_qp, 1e-3003_qp, 1e-3002_qp, 1e-3001_qp, &
        1e-3000_qp, 1e-2999_qp, 1e-2998_qp, 1e-2997_qp, 1e-2996_qp, &
        1e-2995_qp, 1e-2994_qp, 1e-2993_qp, 1e-2992_qp, 1e-2991_qp, &
        1e-2990_qp, 1e-2989_qp, 1e-2988_qp, 1e-2987_qp, 1e-2986_qp, &
        1e-2985_qp, 1e-2984_qp, 1e-2983_qp, 1e-2982_qp, 1e-2981_qp, &
        1e-2980_qp, 1e-2979_qp, 1e-2978_qp, 1e-2977_qp, 1e-2976_qp, &
        1e-2975_qp, 1e-2974_qp, 1e-2973_qp, 1e-2972_qp, 1e-2971_qp, &
        1e-2970_qp, 1e-2969_qp, 1e-2968_qp, 1e-2967_qp, 1e-2966_qp, &
        1e-2965_qp, 1e-2964_qp, 1e-2963_qp, 1e-2962_qp, 1e-2961_qp, &
        1e-2960_qp, 1e-2959_qp, 1e-2958_qp, 1e-2957_qp, 1e-2956_qp, &
        1e-2955_qp, 1e-2954_qp, 1e-2953_qp, 1e-2952_qp, 1e-2951_qp, &
        1e-2950_qp, 1e-2949_qp, 1e-2948_qp, 1e-2947_qp, 1e-2946_qp, &
        1e-2945_qp, 1e-2944_qp, 1e-2943_qp, 1e-2942_qp, 1e-2941_qp, &
        1e-2940_qp, 1e-2939_qp, 1e-2938_qp, 1e-2937_qp, 1e-2936_qp, &
        1e-2935_qp, 1e-2934_qp, 1e-2933_qp, 1e-2932_qp, 1e-2931_qp, &
        1e-2930_qp, 1e-2929_qp, 1e-2928_qp, 1e-2927_qp, 1e-2926_qp, &
        1e-2925_qp, 1e-2924_qp, 1e-2923_qp, 1e-2922_qp, 1e-2921_qp, &
        1e-2920_qp, 1e-2919_qp, 1e-2918_qp, 1e-2917_qp, 1e-2916_qp, &
        1e-2915_qp, 1e-2914_qp, 1e-2913_qp, 1e-2912_qp, 1e-2911_qp, &
        1e-2910_qp, 1e-2909_qp, 1e-2908_qp, 1e-2907_qp, 1e-2906_qp, &
        1e-2905_qp, 1e-2904_qp, 1e-2903_qp, 1e-2902_qp, 1e-2901_qp, &
        1e-2900_qp, 1e-2899_qp, 1e-2898_qp, 1e-2897_qp, 1e-2896_qp, &
        1e-2895_qp, 1e-2894_qp, 1e-2893_qp, 1e-2892_qp, 1e-2891_qp, &
        1e-2890_qp, 1e-2889_qp, 1e-2888_qp, 1e-2887_qp, 1e-2886_qp, &
        1e-2885_qp, 1e-2884_qp, 1e-2883_qp, 1e-2882_qp, 1e-2881_qp, &
        1e-2880_qp, 1e-2879_qp, 1e-2878_qp, 1e-2877_qp, 1e-2876_qp, &
        1e-2875_qp, 1e-2874_qp, 1e-2873_qp, 1e-2872_qp, 1e-2871_qp, &
        1e-2870_qp, 1e-2869_qp, 1e-2868_qp, 1e-2867_qp, 1e-2866_qp, &
        1e-2865_qp, 1e-2864_qp, 1e-2863_qp, 1e-2862_qp, 1e-2861_qp, &
        1e-2860_qp, 1e-2859_qp, 1e-2858_qp, 1e-2857_qp, 1e-2856_qp, &
        1e-2855_qp, 1e-2854_qp, 1e-2853_qp, 1e-2852_qp, 1e-2851_qp, &
        1e-2850_qp, 1e-2849_qp, 1e-2848_qp, 1e-2847_qp, 1e-2846_qp, &
        1e-2845_qp, 1e-2844_qp, 1e-2843_qp, 1e-2842_qp, 1e-2841_qp, &
        1e-2840_qp, 1e-2839_qp, 1e-2838_qp, 1e-2837_qp, 1e-2836_qp, &
        1e-2835_qp, 1e-2834_qp, 1e-2833_qp, 1e-2832_qp, 1e-2831_qp, &
        1e-2830_qp, 1e-2829_qp, 1e-2828_qp, 1e-2827_qp, 1e-2826_qp, &
        1e-2825_qp, 1e-2824_qp, 1e-2823_qp, 1e-2822_qp, 1e-2821_qp, &
        1e-2820_qp, 1e-2819_qp, 1e-2818_qp, 1e-2817_qp, 1e-2816_qp, &
        1e-2815_qp, 1e-2814_qp, 1e-2813_qp, 1e-2812_qp, 1e-2811_qp, &
        1e-2810_qp, 1e-2809_qp, 1e-2808_qp, 1e-2807_qp, 1e-2806_qp, &
        1e-2805_qp, 1e-2804_qp, 1e-2803_qp, 1e-2802_qp, 1e-2801_qp, &
        1e-2800_qp, 1e-2799_qp, 1e-2798_qp, 1e-2797_qp, 1e-2796_qp, &
        1e-2795_qp, 1e-2794_qp, 1e-2793_qp, 1e-2792_qp, 1e-2791_qp, &
        1e-2790_qp, 1e-2789_qp, 1e-2788_qp, 1e-2787_qp, 1e-2786_qp, &
        1e-2785_qp, 1e-2784_qp, 1e-2783_qp, 1e-2782_qp, 1e-2781_qp, &
        1e-2780_qp, 1e-2779_qp, 1e-2778_qp, 1e-2777_qp, 1e-2776_qp, &
        1e-2775_qp, 1e-2774_qp, 1e-2773_qp, 1e-2772_qp, 1e-2771_qp, &
        1e-2770_qp, 1e-2769_qp, 1e-2768_qp, 1e-2767_qp, 1e-2766_qp, &
        1e-2765_qp, 1e-2764_qp, 1e-2763_qp, 1e-2762_qp, 1e-2761_qp, &
        1e-2760_qp, 1e-2759_qp, 1e-2758_qp, 1e-2757_qp, 1e-2756_qp, &
        1e-2755_qp, 1e-2754_qp, 1e-2753_qp, 1e-2752_qp, 1e-2751_qp, &
        1e-2750_qp, 1e-2749_qp, 1e-2748_qp, 1e-2747_qp, 1e-2746_qp, &
        1e-2745_qp, 1e-2744_qp, 1e-2743_qp, 1e-2742_qp, 1e-2741_qp, &
        1e-2740_qp, 1e-2739_qp, 1e-2738_qp, 1e-2737_qp, 1e-2736_qp, &
        1e-2735_qp, 1e-2734_qp, 1e-2733_qp, 1e-2732_qp, 1e-2731_qp, &
        1e-2730_qp, 1e-2729_qp, 1e-2728_qp, 1e-2727_qp, 1e-2726_qp, &
        1e-2725_qp, 1e-2724_qp, 1e-2723_qp, 1e-2722_qp, 1e-2721_qp, &
        1e-2720_qp, 1e-2719_qp, 1e-2718_qp, 1e-2717_qp, 1e-2716_qp, &
        1e-2715_qp, 1e-2714_qp, 1e-2713_qp, 1e-2712_qp, 1e-2711_qp, &
        1e-2710_qp, 1e-2709_qp, 1e-2708_qp, 1e-2707_qp, 1e-2706_qp, &
        1e-2705_qp, 1e-2704_qp, 1e-2703_qp, 1e-2702_qp, 1e-2701_qp, &
        1e-2700_qp, 1e-2699_qp, 1e-2698_qp, 1e-2697_qp, 1e-2696_qp, &
        1e-2695_qp, 1e-2694_qp, 1e-2693_qp, 1e-2692_qp, 1e-2691_qp, &
        1e-2690_qp, 1e-2689_qp, 1e-2688_qp, 1e-2687_qp, 1e-2686_qp, &
        1e-2685_qp, 1e-2684_qp, 1e-2683_qp, 1e-2682_qp, 1e-2681_qp, &
        1e-2680_qp, 1e-2679_qp, 1e-2678_qp, 1e-2677_qp, 1e-2676_qp, &
        1e-2675_qp, 1e-2674_qp, 1e-2673_qp, 1e-2672_qp, 1e-2671_qp, &
        1e-2670_qp, 1e-2669_qp, 1e-2668_qp, 1e-2667_qp, 1e-2666_qp, &
        1e-2665_qp, 1e-2664_qp, 1e-2663_qp, 1e-2662_qp, 1e-2661_qp, &
        1e-2660_qp, 1e-2659_qp, 1e-2658_qp, 1e-2657_qp, 1e-2656_qp, &
        1e-2655_qp, 1e-2654_qp, 1e-2653_qp, 1e-2652_qp, 1e-2651_qp, &
        1e-2650_qp, 1e-2649_qp, 1e-2648_qp, 1e-2647_qp, 1e-2646_qp, &
        1e-2645_qp, 1e-2644_qp, 1e-2643_qp, 1e-2642_qp, 1e-2641_qp, &
        1e-2640_qp, 1e-2639_qp, 1e-2638_qp, 1e-2637_qp, 1e-2636_qp, &
        1e-2635_qp, 1e-2634_qp, 1e-2633_qp, 1e-2632_qp, 1e-2631_qp, &
        1e-2630_qp, 1e-2629_qp, 1e-2628_qp, 1e-2627_qp, 1e-2626_qp, &
        1e-2625_qp, 1e-2624_qp, 1e-2623_qp, 1e-2622_qp, 1e-2621_qp, &
        1e-2620_qp, 1e-2619_qp, 1e-2618_qp, 1e-2617_qp, 1e-2616_qp, &
        1e-2615_qp, 1e-2614_qp, 1e-2613_qp, 1e-2612_qp, 1e-2611_qp, &
        1e-2610_qp, 1e-2609_qp, 1e-2608_qp, 1e-2607_qp, 1e-2606_qp, &
        1e-2605_qp, 1e-2604_qp, 1e-2603_qp, 1e-2602_qp, 1e-2601_qp, &
        1e-2600_qp, 1e-2599_qp, 1e-2598_qp, 1e-2597_qp, 1e-2596_qp, &
        1e-2595_qp, 1e-2594_qp, 1e-2593_qp, 1e-2592_qp, 1e-2591_qp, &
        1e-2590_qp, 1e-2589_qp, 1e-2588_qp, 1e-2587_qp, 1e-2586_qp, &
        1e-2585_qp, 1e-2584_qp, 1e-2583_qp, 1e-2582_qp, 1e-2581_qp, &
        1e-2580_qp, 1e-2579_qp, 1e-2578_qp, 1e-2577_qp, 1e-2576_qp, &
        1e-2575_qp, 1e-2574_qp, 1e-2573_qp, 1e-2572_qp, 1e-2571_qp, &
        1e-2570_qp, 1e-2569_qp, 1e-2568_qp, 1e-2567_qp, 1e-2566_qp, &
        1e-2565_qp, 1e-2564_qp, 1e-2563_qp, 1e-2562_qp, 1e-2561_qp, &
        1e-2560_qp, 1e-2559_qp, 1e-2558_qp, 1e-2557_qp, 1e-2556_qp, &
        1e-2555_qp, 1e-2554_qp, 1e-2553_qp, 1e-2552_qp, 1e-2551_qp, &
        1e-2550_qp, 1e-2549_qp, 1e-2548_qp, 1e-2547_qp, 1e-2546_qp, &
        1e-2545_qp, 1e-2544_qp, 1e-2543_qp, 1e-2542_qp, 1e-2541_qp, &
        1e-2540_qp, 1e-2539_qp, 1e-2538_qp, 1e-2537_qp, 1e-2536_qp, &
        1e-2535_qp, 1e-2534_qp, 1e-2533_qp, 1e-2532_qp, 1e-2531_qp, &
        1e-2530_qp, 1e-2529_qp, 1e-2528_qp, 1e-2527_qp, 1e-2526_qp, &
        1e-2525_qp, 1e-2524_qp, 1e-2523_qp, 1e-2522_qp, 1e-2521_qp, &
        1e-2520_qp, 1e-2519_qp, 1e-2518_qp, 1e-2517_qp, 1e-2516_qp, &
        1e-2515_qp, 1e-2514_qp, 1e-2513_qp, 1e-2512_qp, 1e-2511_qp, &
        1e-2510_qp, 1e-2509_qp, 1e-2508_qp, 1e-2507_qp, 1e-2506_qp, &
        1e-2505_qp, 1e-2504_qp, 1e-2503_qp, 1e-2502_qp, 1e-2501_qp, &
        1e-2500_qp, 1e-2499_qp, 1e-2498_qp, 1e-2497_qp, 1e-2496_qp, &
        1e-2495_qp, 1e-2494_qp, 1e-2493_qp, 1e-2492_qp, 1e-2491_qp, &
        1e-2490_qp, 1e-2489_qp, 1e-2488_qp, 1e-2487_qp, 1e-2486_qp, &
        1e-2485_qp, 1e-2484_qp, 1e-2483_qp, 1e-2482_qp, 1e-2481_qp, &
        1e-2480_qp, 1e-2479_qp, 1e-2478_qp, 1e-2477_qp, 1e-2476_qp, &
        1e-2475_qp, 1e-2474_qp, 1e-2473_qp, 1e-2472_qp, 1e-2471_qp, &
        1e-2470_qp, 1e-2469_qp, 1e-2468_qp, 1e-2467_qp, 1e-2466_qp, &
        1e-2465_qp, 1e-2464_qp, 1e-2463_qp, 1e-2462_qp, 1e-2461_qp, &
        1e-2460_qp, 1e-2459_qp, 1e-2458_qp, 1e-2457_qp, 1e-2456_qp, &
        1e-2455_qp, 1e-2454_qp, 1e-2453_qp, 1e-2452_qp, 1e-2451_qp, &
        1e-2450_qp, 1e-2449_qp, 1e-2448_qp, 1e-2447_qp, 1e-2446_qp, &
        1e-2445_qp, 1e-2444_qp, 1e-2443_qp, 1e-2442_qp, 1e-2441_qp, &
        1e-2440_qp, 1e-2439_qp, 1e-2438_qp, 1e-2437_qp, 1e-2436_qp, &
        1e-2435_qp, 1e-2434_qp, 1e-2433_qp, 1e-2432_qp, 1e-2431_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS3(*) = [ &
        1e-2430_qp, 1e-2429_qp, 1e-2428_qp, 1e-2427_qp, 1e-2426_qp, &
        1e-2425_qp, 1e-2424_qp, 1e-2423_qp, 1e-2422_qp, 1e-2421_qp, &
        1e-2420_qp, 1e-2419_qp, 1e-2418_qp, 1e-2417_qp, 1e-2416_qp, &
        1e-2415_qp, 1e-2414_qp, 1e-2413_qp, 1e-2412_qp, 1e-2411_qp, &
        1e-2410_qp, 1e-2409_qp, 1e-2408_qp, 1e-2407_qp, 1e-2406_qp, &
        1e-2405_qp, 1e-2404_qp, 1e-2403_qp, 1e-2402_qp, 1e-2401_qp, &
        1e-2400_qp, 1e-2399_qp, 1e-2398_qp, 1e-2397_qp, 1e-2396_qp, &
        1e-2395_qp, 1e-2394_qp, 1e-2393_qp, 1e-2392_qp, 1e-2391_qp, &
        1e-2390_qp, 1e-2389_qp, 1e-2388_qp, 1e-2387_qp, 1e-2386_qp, &
        1e-2385_qp, 1e-2384_qp, 1e-2383_qp, 1e-2382_qp, 1e-2381_qp, &
        1e-2380_qp, 1e-2379_qp, 1e-2378_qp, 1e-2377_qp, 1e-2376_qp, &
        1e-2375_qp, 1e-2374_qp, 1e-2373_qp, 1e-2372_qp, 1e-2371_qp, &
        1e-2370_qp, 1e-2369_qp, 1e-2368_qp, 1e-2367_qp, 1e-2366_qp, &
        1e-2365_qp, 1e-2364_qp, 1e-2363_qp, 1e-2362_qp, 1e-2361_qp, &
        1e-2360_qp, 1e-2359_qp, 1e-2358_qp, 1e-2357_qp, 1e-2356_qp, &
        1e-2355_qp, 1e-2354_qp, 1e-2353_qp, 1e-2352_qp, 1e-2351_qp, &
        1e-2350_qp, 1e-2349_qp, 1e-2348_qp, 1e-2347_qp, 1e-2346_qp, &
        1e-2345_qp, 1e-2344_qp, 1e-2343_qp, 1e-2342_qp, 1e-2341_qp, &
        1e-2340_qp, 1e-2339_qp, 1e-2338_qp, 1e-2337_qp, 1e-2336_qp, &
        1e-2335_qp, 1e-2334_qp, 1e-2333_qp, 1e-2332_qp, 1e-2331_qp, &
        1e-2330_qp, 1e-2329_qp, 1e-2328_qp, 1e-2327_qp, 1e-2326_qp, &
        1e-2325_qp, 1e-2324_qp, 1e-2323_qp, 1e-2322_qp, 1e-2321_qp, &
        1e-2320_qp, 1e-2319_qp, 1e-2318_qp, 1e-2317_qp, 1e-2316_qp, &
        1e-2315_qp, 1e-2314_qp, 1e-2313_qp, 1e-2312_qp, 1e-2311_qp, &
        1e-2310_qp, 1e-2309_qp, 1e-2308_qp, 1e-2307_qp, 1e-2306_qp, &
        1e-2305_qp, 1e-2304_qp, 1e-2303_qp, 1e-2302_qp, 1e-2301_qp, &
        1e-2300_qp, 1e-2299_qp, 1e-2298_qp, 1e-2297_qp, 1e-2296_qp, &
        1e-2295_qp, 1e-2294_qp, 1e-2293_qp, 1e-2292_qp, 1e-2291_qp, &
        1e-2290_qp, 1e-2289_qp, 1e-2288_qp, 1e-2287_qp, 1e-2286_qp, &
        1e-2285_qp, 1e-2284_qp, 1e-2283_qp, 1e-2282_qp, 1e-2281_qp, &
        1e-2280_qp, 1e-2279_qp, 1e-2278_qp, 1e-2277_qp, 1e-2276_qp, &
        1e-2275_qp, 1e-2274_qp, 1e-2273_qp, 1e-2272_qp, 1e-2271_qp, &
        1e-2270_qp, 1e-2269_qp, 1e-2268_qp, 1e-2267_qp, 1e-2266_qp, &
        1e-2265_qp, 1e-2264_qp, 1e-2263_qp, 1e-2262_qp, 1e-2261_qp, &
        1e-2260_qp, 1e-2259_qp, 1e-2258_qp, 1e-2257_qp, 1e-2256_qp, &
        1e-2255_qp, 1e-2254_qp, 1e-2253_qp, 1e-2252_qp, 1e-2251_qp, &
        1e-2250_qp, 1e-2249_qp, 1e-2248_qp, 1e-2247_qp, 1e-2246_qp, &
        1e-2245_qp, 1e-2244_qp, 1e-2243_qp, 1e-2242_qp, 1e-2241_qp, &
        1e-2240_qp, 1e-2239_qp, 1e-2238_qp, 1e-2237_qp, 1e-2236_qp, &
        1e-2235_qp, 1e-2234_qp, 1e-2233_qp, 1e-2232_qp, 1e-2231_qp, &
        1e-2230_qp, 1e-2229_qp, 1e-2228_qp, 1e-2227_qp, 1e-2226_qp, &
        1e-2225_qp, 1e-2224_qp, 1e-2223_qp, 1e-2222_qp, 1e-2221_qp, &
        1e-2220_qp, 1e-2219_qp, 1e-2218_qp, 1e-2217_qp, 1e-2216_qp, &
        1e-2215_qp, 1e-2214_qp, 1e-2213_qp, 1e-2212_qp, 1e-2211_qp, &
        1e-2210_qp, 1e-2209_qp, 1e-2208_qp, 1e-2207_qp, 1e-2206_qp, &
        1e-2205_qp, 1e-2204_qp, 1e-2203_qp, 1e-2202_qp, 1e-2201_qp, &
        1e-2200_qp, 1e-2199_qp, 1e-2198_qp, 1e-2197_qp, 1e-2196_qp, &
        1e-2195_qp, 1e-2194_qp, 1e-2193_qp, 1e-2192_qp, 1e-2191_qp, &
        1e-2190_qp, 1e-2189_qp, 1e-2188_qp, 1e-2187_qp, 1e-2186_qp, &
        1e-2185_qp, 1e-2184_qp, 1e-2183_qp, 1e-2182_qp, 1e-2181_qp, &
        1e-2180_qp, 1e-2179_qp, 1e-2178_qp, 1e-2177_qp, 1e-2176_qp, &
        1e-2175_qp, 1e-2174_qp, 1e-2173_qp, 1e-2172_qp, 1e-2171_qp, &
        1e-2170_qp, 1e-2169_qp, 1e-2168_qp, 1e-2167_qp, 1e-2166_qp, &
        1e-2165_qp, 1e-2164_qp, 1e-2163_qp, 1e-2162_qp, 1e-2161_qp, &
        1e-2160_qp, 1e-2159_qp, 1e-2158_qp, 1e-2157_qp, 1e-2156_qp, &
        1e-2155_qp, 1e-2154_qp, 1e-2153_qp, 1e-2152_qp, 1e-2151_qp, &
        1e-2150_qp, 1e-2149_qp, 1e-2148_qp, 1e-2147_qp, 1e-2146_qp, &
        1e-2145_qp, 1e-2144_qp, 1e-2143_qp, 1e-2142_qp, 1e-2141_qp, &
        1e-2140_qp, 1e-2139_qp, 1e-2138_qp, 1e-2137_qp, 1e-2136_qp, &
        1e-2135_qp, 1e-2134_qp, 1e-2133_qp, 1e-2132_qp, 1e-2131_qp, &
        1e-2130_qp, 1e-2129_qp, 1e-2128_qp, 1e-2127_qp, 1e-2126_qp, &
        1e-2125_qp, 1e-2124_qp, 1e-2123_qp, 1e-2122_qp, 1e-2121_qp, &
        1e-2120_qp, 1e-2119_qp, 1e-2118_qp, 1e-2117_qp, 1e-2116_qp, &
        1e-2115_qp, 1e-2114_qp, 1e-2113_qp, 1e-2112_qp, 1e-2111_qp, &
        1e-2110_qp, 1e-2109_qp, 1e-2108_qp, 1e-2107_qp, 1e-2106_qp, &
        1e-2105_qp, 1e-2104_qp, 1e-2103_qp, 1e-2102_qp, 1e-2101_qp, &
        1e-2100_qp, 1e-2099_qp, 1e-2098_qp, 1e-2097_qp, 1e-2096_qp, &
        1e-2095_qp, 1e-2094_qp, 1e-2093_qp, 1e-2092_qp, 1e-2091_qp, &
        1e-2090_qp, 1e-2089_qp, 1e-2088_qp, 1e-2087_qp, 1e-2086_qp, &
        1e-2085_qp, 1e-2084_qp, 1e-2083_qp, 1e-2082_qp, 1e-2081_qp, &
        1e-2080_qp, 1e-2079_qp, 1e-2078_qp, 1e-2077_qp, 1e-2076_qp, &
        1e-2075_qp, 1e-2074_qp, 1e-2073_qp, 1e-2072_qp, 1e-2071_qp, &
        1e-2070_qp, 1e-2069_qp, 1e-2068_qp, 1e-2067_qp, 1e-2066_qp, &
        1e-2065_qp, 1e-2064_qp, 1e-2063_qp, 1e-2062_qp, 1e-2061_qp, &
        1e-2060_qp, 1e-2059_qp, 1e-2058_qp, 1e-2057_qp, 1e-2056_qp, &
        1e-2055_qp, 1e-2054_qp, 1e-2053_qp, 1e-2052_qp, 1e-2051_qp, &
        1e-2050_qp, 1e-2049_qp, 1e-2048_qp, 1e-2047_qp, 1e-2046_qp, &
        1e-2045_qp, 1e-2044_qp, 1e-2043_qp, 1e-2042_qp, 1e-2041_qp, &
        1e-2040_qp, 1e-2039_qp, 1e-2038_qp, 1e-2037_qp, 1e-2036_qp, &
        1e-2035_qp, 1e-2034_qp, 1e-2033_qp, 1e-2032_qp, 1e-2031_qp, &
        1e-2030_qp, 1e-2029_qp, 1e-2028_qp, 1e-2027_qp, 1e-2026_qp, &
        1e-2025_qp, 1e-2024_qp, 1e-2023_qp, 1e-2022_qp, 1e-2021_qp, &
        1e-2020_qp, 1e-2019_qp, 1e-2018_qp, 1e-2017_qp, 1e-2016_qp, &
        1e-2015_qp, 1e-2014_qp, 1e-2013_qp, 1e-2012_qp, 1e-2011_qp, &
        1e-2010_qp, 1e-2009_qp, 1e-2008_qp, 1e-2007_qp, 1e-2006_qp, &
        1e-2005_qp, 1e-2004_qp, 1e-2003_qp, 1e-2002_qp, 1e-2001_qp, &
        1e-2000_qp, 1e-1999_qp, 1e-1998_qp, 1e-1997_qp, 1e-1996_qp, &
        1e-1995_qp, 1e-1994_qp, 1e-1993_qp, 1e-1992_qp, 1e-1991_qp, &
        1e-1990_qp, 1e-1989_qp, 1e-1988_qp, 1e-1987_qp, 1e-1986_qp, &
        1e-1985_qp, 1e-1984_qp, 1e-1983_qp, 1e-1982_qp, 1e-1981_qp, &
        1e-1980_qp, 1e-1979_qp, 1e-1978_qp, 1e-1977_qp, 1e-1976_qp, &
        1e-1975_qp, 1e-1974_qp, 1e-1973_qp, 1e-1972_qp, 1e-1971_qp, &
        1e-1970_qp, 1e-1969_qp, 1e-1968_qp, 1e-1967_qp, 1e-1966_qp, &
        1e-1965_qp, 1e-1964_qp, 1e-1963_qp, 1e-1962_qp, 1e-1961_qp, &
        1e-1960_qp, 1e-1959_qp, 1e-1958_qp, 1e-1957_qp, 1e-1956_qp, &
        1e-1955_qp, 1e-1954_qp, 1e-1953_qp, 1e-1952_qp, 1e-1951_qp, &
        1e-1950_qp, 1e-1949_qp, 1e-1948_qp, 1e-1947_qp, 1e-1946_qp, &
        1e-1945_qp, 1e-1944_qp, 1e-1943_qp, 1e-1942_qp, 1e-1941_qp, &
        1e-1940_qp, 1e-1939_qp, 1e-1938_qp, 1e-1937_qp, 1e-1936_qp, &
        1e-1935_qp, 1e-1934_qp, 1e-1933_qp, 1e-1932_qp, 1e-1931_qp, &
        1e-1930_qp, 1e-1929_qp, 1e-1928_qp, 1e-1927_qp, 1e-1926_qp, &
        1e-1925_qp, 1e-1924_qp, 1e-1923_qp, 1e-1922_qp, 1e-1921_qp, &
        1e-1920_qp, 1e-1919_qp, 1e-1918_qp, 1e-1917_qp, 1e-1916_qp, &
        1e-1915_qp, 1e-1914_qp, 1e-1913_qp, 1e-1912_qp, 1e-1911_qp, &
        1e-1910_qp, 1e-1909_qp, 1e-1908_qp, 1e-1907_qp, 1e-1906_qp, &
        1e-1905_qp, 1e-1904_qp, 1e-1903_qp, 1e-1902_qp, 1e-1901_qp, &
        1e-1900_qp, 1e-1899_qp, 1e-1898_qp, 1e-1897_qp, 1e-1896_qp, &
        1e-1895_qp, 1e-1894_qp, 1e-1893_qp, 1e-1892_qp, 1e-1891_qp, &
        1e-1890_qp, 1e-1889_qp, 1e-1888_qp, 1e-1887_qp, 1e-1886_qp, &
        1e-1885_qp, 1e-1884_qp, 1e-1883_qp, 1e-1882_qp, 1e-1881_qp, &
        1e-1880_qp, 1e-1879_qp, 1e-1878_qp, 1e-1877_qp, 1e-1876_qp, &
        1e-1875_qp, 1e-1874_qp, 1e-1873_qp, 1e-1872_qp, 1e-1871_qp, &
        1e-1870_qp, 1e-1869_qp, 1e-1868_qp, 1e-1867_qp, 1e-1866_qp, &
        1e-1865_qp, 1e-1864_qp, 1e-1863_qp, 1e-1862_qp, 1e-1861_qp, &
        1e-1860_qp, 1e-1859_qp, 1e-1858_qp, 1e-1857_qp, 1e-1856_qp, &
        1e-1855_qp, 1e-1854_qp, 1e-1853_qp, 1e-1852_qp, 1e-1851_qp, &
        1e-1850_qp, 1e-1849_qp, 1e-1848_qp, 1e-1847_qp, 1e-1846_qp, &
        1e-1845_qp, 1e-1844_qp, 1e-1843_qp, 1e-1842_qp, 1e-1841_qp, &
        1e-1840_qp, 1e-1839_qp, 1e-1838_qp, 1e-1837_qp, 1e-1836_qp, &
        1e-1835_qp, 1e-1834_qp, 1e-1833_qp, 1e-1832_qp, 1e-1831_qp, &
        1e-1830_qp, 1e-1829_qp, 1e-1828_qp, 1e-1827_qp, 1e-1826_qp, &
        1e-1825_qp, 1e-1824_qp, 1e-1823_qp, 1e-1822_qp, 1e-1821_qp, &
        1e-1820_qp, 1e-1819_qp, 1e-1818_qp, 1e-1817_qp, 1e-1816_qp, &
        1e-1815_qp, 1e-1814_qp, 1e-1813_qp, 1e-1812_qp, 1e-1811_qp, &
        1e-1810_qp, 1e-1809_qp, 1e-1808_qp, 1e-1807_qp, 1e-1806_qp, &
        1e-1805_qp, 1e-1804_qp, 1e-1803_qp, 1e-1802_qp, 1e-1801_qp, &
        1e-1800_qp, 1e-1799_qp, 1e-1798_qp, 1e-1797_qp, 1e-1796_qp, &
        1e-1795_qp, 1e-1794_qp, 1e-1793_qp, 1e-1792_qp, 1e-1791_qp, &
        1e-1790_qp, 1e-1789_qp, 1e-1788_qp, 1e-1787_qp, 1e-1786_qp, &
        1e-1785_qp, 1e-1784_qp, 1e-1783_qp, 1e-1782_qp, 1e-1781_qp, &
        1e-1780_qp, 1e-1779_qp, 1e-1778_qp, 1e-1777_qp, 1e-1776_qp, &
        1e-1775_qp, 1e-1774_qp, 1e-1773_qp, 1e-1772_qp, 1e-1771_qp, &
        1e-1770_qp, 1e-1769_qp, 1e-1768_qp, 1e-1767_qp, 1e-1766_qp, &
        1e-1765_qp, 1e-1764_qp, 1e-1763_qp, 1e-1762_qp, 1e-1761_qp, &
        1e-1760_qp, 1e-1759_qp, 1e-1758_qp, 1e-1757_qp, 1e-1756_qp, &
        1e-1755_qp, 1e-1754_qp, 1e-1753_qp, 1e-1752_qp, 1e-1751_qp, &
        1e-1750_qp, 1e-1749_qp, 1e-1748_qp, 1e-1747_qp, 1e-1746_qp, &
        1e-1745_qp, 1e-1744_qp, 1e-1743_qp, 1e-1742_qp, 1e-1741_qp, &
        1e-1740_qp, 1e-1739_qp, 1e-1738_qp, 1e-1737_qp, 1e-1736_qp, &
        1e-1735_qp, 1e-1734_qp, 1e-1733_qp, 1e-1732_qp, 1e-1731_qp, &
        1e-1730_qp, 1e-1729_qp, 1e-1728_qp, 1e-1727_qp, 1e-1726_qp, &
        1e-1725_qp, 1e-1724_qp, 1e-1723_qp, 1e-1722_qp, 1e-1721_qp, &
        1e-1720_qp, 1e-1719_qp, 1e-1718_qp, 1e-1717_qp, 1e-1716_qp, &
        1e-1715_qp, 1e-1714_qp, 1e-1713_qp, 1e-1712_qp, 1e-1711_qp, &
        1e-1710_qp, 1e-1709_qp, 1e-1708_qp, 1e-1707_qp, 1e-1706_qp, &
        1e-1705_qp, 1e-1704_qp, 1e-1703_qp, 1e-1702_qp, 1e-1701_qp, &
        1e-1700_qp, 1e-1699_qp, 1e-1698_qp, 1e-1697_qp, 1e-1696_qp, &
        1e-1695_qp, 1e-1694_qp, 1e-1693_qp, 1e-1692_qp, 1e-1691_qp, &
        1e-1690_qp, 1e-1689_qp, 1e-1688_qp, 1e-1687_qp, 1e-1686_qp, &
        1e-1685_qp, 1e-1684_qp, 1e-1683_qp, 1e-1682_qp, 1e-1681_qp, &
        1e-1680_qp, 1e-1679_qp, 1e-1678_qp, 1e-1677_qp, 1e-1676_qp, &
        1e-1675_qp, 1e-1674_qp, 1e-1673_qp, 1e-1672_qp, 1e-1671_qp, &
        1e-1670_qp, 1e-1669_qp, 1e-1668_qp, 1e-1667_qp, 1e-1666_qp, &
        1e-1665_qp, 1e-1664_qp, 1e-1663_qp, 1e-1662_qp, 1e-1661_qp, &
        1e-1660_qp, 1e-1659_qp, 1e-1658_qp, 1e-1657_qp, 1e-1656_qp, &
        1e-1655_qp, 1e-1654_qp, 1e-1653_qp, 1e-1652_qp, 1e-1651_qp, &
        1e-1650_qp, 1e-1649_qp, 1e-1648_qp, 1e-1647_qp, 1e-1646_qp, &
        1e-1645_qp, 1e-1644_qp, 1e-1643_qp, 1e-1642_qp, 1e-1641_qp, &
        1e-1640_qp, 1e-1639_qp, 1e-1638_qp, 1e-1637_qp, 1e-1636_qp, &
        1e-1635_qp, 1e-1634_qp, 1e-1633_qp, 1e-1632_qp, 1e-1631_qp, &
        1e-1630_qp, 1e-1629_qp, 1e-1628_qp, 1e-1627_qp, 1e-1626_qp, &
        1e-1625_qp, 1e-1624_qp, 1e-1623_qp, 1e-1622_qp, 1e-1621_qp, &
        1e-1620_qp, 1e-1619_qp, 1e-1618_qp, 1e-1617_qp, 1e-1616_qp, &
        1e-1615_qp, 1e-1614_qp, 1e-1613_qp, 1e-1612_qp, 1e-1611_qp, &
        1e-1610_qp, 1e-1609_qp, 1e-1608_qp, 1e-1607_qp, 1e-1606_qp, &
        1e-1605_qp, 1e-1604_qp, 1e-1603_qp, 1e-1602_qp, 1e-1601_qp, &
        1e-1600_qp, 1e-1599_qp, 1e-1598_qp, 1e-1597_qp, 1e-1596_qp, &
        1e-1595_qp, 1e-1594_qp, 1e-1593_qp, 1e-1592_qp, 1e-1591_qp, &
        1e-1590_qp, 1e-1589_qp, 1e-1588_qp, 1e-1587_qp, 1e-1586_qp, &
        1e-1585_qp, 1e-1584_qp, 1e-1583_qp, 1e-1582_qp, 1e-1581_qp, &
        1e-1580_qp, 1e-1579_qp, 1e-1578_qp, 1e-1577_qp, 1e-1576_qp, &
        1e-1575_qp, 1e-1574_qp, 1e-1573_qp, 1e-1572_qp, 1e-1571_qp, &
        1e-1570_qp, 1e-1569_qp, 1e-1568_qp, 1e-1567_qp, 1e-1566_qp, &
        1e-1565_qp, 1e-1564_qp, 1e-1563_qp, 1e-1562_qp, 1e-1561_qp, &
        1e-1560_qp, 1e-1559_qp, 1e-1558_qp, 1e-1557_qp, 1e-1556_qp, &
        1e-1555_qp, 1e-1554_qp, 1e-1553_qp, 1e-1552_qp, 1e-1551_qp, &
        1e-1550_qp, 1e-1549_qp, 1e-1548_qp, 1e-1547_qp, 1e-1546_qp, &
        1e-1545_qp, 1e-1544_qp, 1e-1543_qp, 1e-1542_qp, 1e-1541_qp, &
        1e-1540_qp, 1e-1539_qp, 1e-1538_qp, 1e-1537_qp, 1e-1536_qp, &
        1e-1535_qp, 1e-1534_qp, 1e-1533_qp, 1e-1532_qp, 1e-1531_qp, &
        1e-1530_qp, 1e-1529_qp, 1e-1528_qp, 1e-1527_qp, 1e-1526_qp, &
        1e-1525_qp, 1e-1524_qp, 1e-1523_qp, 1e-1522_qp, 1e-1521_qp, &
        1e-1520_qp, 1e-1519_qp, 1e-1518_qp, 1e-1517_qp, 1e-1516_qp, &
        1e-1515_qp, 1e-1514_qp, 1e-1513_qp, 1e-1512_qp, 1e-1511_qp, &
        1e-1510_qp, 1e-1509_qp, 1e-1508_qp, 1e-1507_qp, 1e-1506_qp, &
        1e-1505_qp, 1e-1504_qp, 1e-1503_qp, 1e-1502_qp, 1e-1501_qp, &
        1e-1500_qp, 1e-1499_qp, 1e-1498_qp, 1e-1497_qp, 1e-1496_qp, &
        1e-1495_qp, 1e-1494_qp, 1e-1493_qp, 1e-1492_qp, 1e-1491_qp, &
        1e-1490_qp, 1e-1489_qp, 1e-1488_qp, 1e-1487_qp, 1e-1486_qp, &
        1e-1485_qp, 1e-1484_qp, 1e-1483_qp, 1e-1482_qp, 1e-1481_qp, &
        1e-1480_qp, 1e-1479_qp, 1e-1478_qp, 1e-1477_qp, 1e-1476_qp, &
        1e-1475_qp, 1e-1474_qp, 1e-1473_qp, 1e-1472_qp, 1e-1471_qp, &
        1e-1470_qp, 1e-1469_qp, 1e-1468_qp, 1e-1467_qp, 1e-1466_qp, &
        1e-1465_qp, 1e-1464_qp, 1e-1463_qp, 1e-1462_qp, 1e-1461_qp, &
        1e-1460_qp, 1e-1459_qp, 1e-1458_qp, 1e-1457_qp, 1e-1456_qp, &
        1e-1455_qp, 1e-1454_qp, 1e-1453_qp, 1e-1452_qp, 1e-1451_qp, &
        1e-1450_qp, 1e-1449_qp, 1e-1448_qp, 1e-1447_qp, 1e-1446_qp, &
        1e-1445_qp, 1e-1444_qp, 1e-1443_qp, 1e-1442_qp, 1e-1441_qp, &
        1e-1440_qp, 1e-1439_qp, 1e-1438_qp, 1e-1437_qp, 1e-1436_qp, &
        1e-1435_qp, 1e-1434_qp, 1e-1433_qp, 1e-1432_qp, 1e-1431_qp, &
        1e-1430_qp, 1e-1429_qp, 1e-1428_qp, 1e-1427_qp, 1e-1426_qp, &
        1e-1425_qp, 1e-1424_qp, 1e-1423_qp, 1e-1422_qp, 1e-1421_qp, &
        1e-1420_qp, 1e-1419_qp, 1e-1418_qp, 1e-1417_qp, 1e-1416_qp, &
        1e-1415_qp, 1e-1414_qp, 1e-1413_qp, 1e-1412_qp, 1e-1411_qp, &
        1e-1410_qp, 1e-1409_qp, 1e-1408_qp, 1e-1407_qp, 1e-1406_qp, &
        1e-1405_qp, 1e-1404_qp, 1e-1403_qp, 1e-1402_qp, 1e-1401_qp, &
        1e-1400_qp, 1e-1399_qp, 1e-1398_qp, 1e-1397_qp, 1e-1396_qp, &
        1e-1395_qp, 1e-1394_qp, 1e-1393_qp, 1e-1392_qp, 1e-1391_qp, &
        1e-1390_qp, 1e-1389_qp, 1e-1388_qp, 1e-1387_qp, 1e-1386_qp, &
        1e-1385_qp, 1e-1384_qp, 1e-1383_qp, 1e-1382_qp, 1e-1381_qp, &
        1e-1380_qp, 1e-1379_qp, 1e-1378_qp, 1e-1377_qp, 1e-1376_qp, &
        1e-1375_qp, 1e-1374_qp, 1e-1373_qp, 1e-1372_qp, 1e-1371_qp, &
        1e-1370_qp, 1e-1369_qp, 1e-1368_qp, 1e-1367_qp, 1e-1366_qp, &
        1e-1365_qp, 1e-1364_qp, 1e-1363_qp, 1e-1362_qp, 1e-1361_qp, &
        1e-1360_qp, 1e-1359_qp, 1e-1358_qp, 1e-1357_qp, 1e-1356_qp, &
        1e-1355_qp, 1e-1354_qp, 1e-1353_qp, 1e-1352_qp, 1e-1351_qp, &
        1e-1350_qp, 1e-1349_qp, 1e-1348_qp, 1e-1347_qp, 1e-1346_qp, &
        1e-1345_qp, 1e-1344_qp, 1e-1343_qp, 1e-1342_qp, 1e-1341_qp, &
        1e-1340_qp, 1e-1339_qp, 1e-1338_qp, 1e-1337_qp, 1e-1336_qp, &
        1e-1335_qp, 1e-1334_qp, 1e-1333_qp, 1e-1332_qp, 1e-1331_qp, &
        1e-1330_qp, 1e-1329_qp, 1e-1328_qp, 1e-1327_qp, 1e-1326_qp, &
        1e-1325_qp, 1e-1324_qp, 1e-1323_qp, 1e-1322_qp, 1e-1321_qp, &
        1e-1320_qp, 1e-1319_qp, 1e-1318_qp, 1e-1317_qp, 1e-1316_qp, &
        1e-1315_qp, 1e-1314_qp, 1e-1313_qp, 1e-1312_qp, 1e-1311_qp, &
        1e-1310_qp, 1e-1309_qp, 1e-1308_qp, 1e-1307_qp, 1e-1306_qp, &
        1e-1305_qp, 1e-1304_qp, 1e-1303_qp, 1e-1302_qp, 1e-1301_qp, &
        1e-1300_qp, 1e-1299_qp, 1e-1298_qp, 1e-1297_qp, 1e-1296_qp, &
        1e-1295_qp, 1e-1294_qp, 1e-1293_qp, 1e-1292_qp, 1e-1291_qp, &
        1e-1290_qp, 1e-1289_qp, 1e-1288_qp, 1e-1287_qp, 1e-1286_qp, &
        1e-1285_qp, 1e-1284_qp, 1e-1283_qp, 1e-1282_qp, 1e-1281_qp, &
        1e-1280_qp, 1e-1279_qp, 1e-1278_qp, 1e-1277_qp, 1e-1276_qp, &
        1e-1275_qp, 1e-1274_qp, 1e-1273_qp, 1e-1272_qp, 1e-1271_qp, &
        1e-1270_qp, 1e-1269_qp, 1e-1268_qp, 1e-1267_qp, 1e-1266_qp, &
        1e-1265_qp, 1e-1264_qp, 1e-1263_qp, 1e-1262_qp, 1e-1261_qp, &
        1e-1260_qp, 1e-1259_qp, 1e-1258_qp, 1e-1257_qp, 1e-1256_qp, &
        1e-1255_qp, 1e-1254_qp, 1e-1253_qp, 1e-1252_qp, 1e-1251_qp, &
        1e-1250_qp, 1e-1249_qp, 1e-1248_qp, 1e-1247_qp, 1e-1246_qp, &
        1e-1245_qp, 1e-1244_qp, 1e-1243_qp, 1e-1242_qp, 1e-1241_qp, &
        1e-1240_qp, 1e-1239_qp, 1e-1238_qp, 1e-1237_qp, 1e-1236_qp, &
        1e-1235_qp, 1e-1234_qp, 1e-1233_qp, 1e-1232_qp, 1e-1231_qp, &
        1e-1230_qp, 1e-1229_qp, 1e-1228_qp, 1e-1227_qp, 1e-1226_qp, &
        1e-1225_qp, 1e-1224_qp, 1e-1223_qp, 1e-1222_qp, 1e-1221_qp, &
        1e-1220_qp, 1e-1219_qp, 1e-1218_qp, 1e-1217_qp, 1e-1216_qp, &
        1e-1215_qp, 1e-1214_qp, 1e-1213_qp, 1e-1212_qp, 1e-1211_qp, &
        1e-1210_qp, 1e-1209_qp, 1e-1208_qp, 1e-1207_qp, 1e-1206_qp, &
        1e-1205_qp, 1e-1204_qp, 1e-1203_qp, 1e-1202_qp, 1e-1201_qp, &
        1e-1200_qp, 1e-1199_qp, 1e-1198_qp, 1e-1197_qp, 1e-1196_qp, &
        1e-1195_qp, 1e-1194_qp, 1e-1193_qp, 1e-1192_qp, 1e-1191_qp, &
        1e-1190_qp, 1e-1189_qp, 1e-1188_qp, 1e-1187_qp, 1e-1186_qp, &
        1e-1185_qp, 1e-1184_qp, 1e-1183_qp, 1e-1182_qp, 1e-1181_qp, &
        1e-1180_qp, 1e-1179_qp, 1e-1178_qp, 1e-1177_qp, 1e-1176_qp, &
        1e-1175_qp, 1e-1174_qp, 1e-1173_qp, 1e-1172_qp, 1e-1171_qp, &
        1e-1170_qp, 1e-1169_qp, 1e-1168_qp, 1e-1167_qp, 1e-1166_qp, &
        1e-1165_qp, 1e-1164_qp, 1e-1163_qp, 1e-1162_qp, 1e-1161_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS4(*) = [ &
        1e-1160_qp, 1e-1159_qp, 1e-1158_qp, 1e-1157_qp, 1e-1156_qp, &
        1e-1155_qp, 1e-1154_qp, 1e-1153_qp, 1e-1152_qp, 1e-1151_qp, &
        1e-1150_qp, 1e-1149_qp, 1e-1148_qp, 1e-1147_qp, 1e-1146_qp, &
        1e-1145_qp, 1e-1144_qp, 1e-1143_qp, 1e-1142_qp, 1e-1141_qp, &
        1e-1140_qp, 1e-1139_qp, 1e-1138_qp, 1e-1137_qp, 1e-1136_qp, &
        1e-1135_qp, 1e-1134_qp, 1e-1133_qp, 1e-1132_qp, 1e-1131_qp, &
        1e-1130_qp, 1e-1129_qp, 1e-1128_qp, 1e-1127_qp, 1e-1126_qp, &
        1e-1125_qp, 1e-1124_qp, 1e-1123_qp, 1e-1122_qp, 1e-1121_qp, &
        1e-1120_qp, 1e-1119_qp, 1e-1118_qp, 1e-1117_qp, 1e-1116_qp, &
        1e-1115_qp, 1e-1114_qp, 1e-1113_qp, 1e-1112_qp, 1e-1111_qp, &
        1e-1110_qp, 1e-1109_qp, 1e-1108_qp, 1e-1107_qp, 1e-1106_qp, &
        1e-1105_qp, 1e-1104_qp, 1e-1103_qp, 1e-1102_qp, 1e-1101_qp, &
        1e-1100_qp, 1e-1099_qp, 1e-1098_qp, 1e-1097_qp, 1e-1096_qp, &
        1e-1095_qp, 1e-1094_qp, 1e-1093_qp, 1e-1092_qp, 1e-1091_qp, &
        1e-1090_qp, 1e-1089_qp, 1e-1088_qp, 1e-1087_qp, 1e-1086_qp, &
        1e-1085_qp, 1e-1084_qp, 1e-1083_qp, 1e-1082_qp, 1e-1081_qp, &
        1e-1080_qp, 1e-1079_qp, 1e-1078_qp, 1e-1077_qp, 1e-1076_qp, &
        1e-1075_qp, 1e-1074_qp, 1e-1073_qp, 1e-1072_qp, 1e-1071_qp, &
        1e-1070_qp, 1e-1069_qp, 1e-1068_qp, 1e-1067_qp, 1e-1066_qp, &
        1e-1065_qp, 1e-1064_qp, 1e-1063_qp, 1e-1062_qp, 1e-1061_qp, &
        1e-1060_qp, 1e-1059_qp, 1e-1058_qp, 1e-1057_qp, 1e-1056_qp, &
        1e-1055_qp, 1e-1054_qp, 1e-1053_qp, 1e-1052_qp, 1e-1051_qp, &
        1e-1050_qp, 1e-1049_qp, 1e-1048_qp, 1e-1047_qp, 1e-1046_qp, &
        1e-1045_qp, 1e-1044_qp, 1e-1043_qp, 1e-1042_qp, 1e-1041_qp, &
        1e-1040_qp, 1e-1039_qp, 1e-1038_qp, 1e-1037_qp, 1e-1036_qp, &
        1e-1035_qp, 1e-1034_qp, 1e-1033_qp, 1e-1032_qp, 1e-1031_qp, &
        1e-1030_qp, 1e-1029_qp, 1e-1028_qp, 1e-1027_qp, 1e-1026_qp, &
        1e-1025_qp, 1e-1024_qp, 1e-1023_qp, 1e-1022_qp, 1e-1021_qp, &
        1e-1020_qp, 1e-1019_qp, 1e-1018_qp, 1e-1017_qp, 1e-1016_qp, &
        1e-1015_qp, 1e-1014_qp, 1e-1013_qp, 1e-1012_qp, 1e-1011_qp, &
        1e-1010_qp, 1e-1009_qp, 1e-1008_qp, 1e-1007_qp, 1e-1006_qp, &
        1e-1005_qp, 1e-1004_qp, 1e-1003_qp, 1e-1002_qp, 1e-1001_qp, &
        1e-1000_qp, 1e-999_qp, 1e-998_qp, 1e-997_qp, 1e-996_qp, 1e-995_qp, &
        1e-994_qp, 1e-993_qp, 1e-992_qp, 1e-991_qp, 1e-990_qp, 1e-989_qp, &
        1e-988_qp, 1e-987_qp, 1e-986_qp, 1e-985_qp, 1e-984_qp, 1e-983_qp, &
        1e-982_qp, 1e-981_qp, 1e-980_qp, 1e-979_qp, 1e-978_qp, 1e-977_qp, &
        1e-976_qp, 1e-975_qp, 1e-974_qp, 1e-973_qp, 1e-972_qp, 1e-971_qp, &
        1e-970_qp, 1e-969_qp, 1e-968_qp, 1e-967_qp, 1e-966_qp, 1e-965_qp, &
        1e-964_qp, 1e-963_qp, 1e-962_qp, 1e-961_qp, 1e-960_qp, 1e-959_qp, &
        1e-958_qp, 1e-957_qp, 1e-956_qp, 1e-955_qp, 1e-954_qp, 1e-953_qp, &
        1e-952_qp, 1e-951_qp, 1e-950_qp, 1e-949_qp, 1e-948_qp, 1e-947_qp, &
        1e-946_qp, 1e-945_qp, 1e-944_qp, 1e-943_qp, 1e-942_qp, 1e-941_qp, &
        1e-940_qp, 1e-939_qp, 1e-938_qp, 1e-937_qp, 1e-936_qp, 1e-935_qp, &
        1e-934_qp, 1e-933_qp, 1e-932_qp, 1e-931_qp, 1e-930_qp, 1e-929_qp, &
        1e-928_qp, 1e-927_qp, 1e-926_qp, 1e-925_qp, 1e-924_qp, 1e-923_qp, &
        1e-922_qp, 1e-921_qp, 1e-920_qp, 1e-919_qp, 1e-918_qp, 1e-917_qp, &
        1e-916_qp, 1e-915_qp, 1e-914_qp, 1e-913_qp, 1e-912_qp, 1e-911_qp, &
        1e-910_qp, 1e-909_qp, 1e-908_qp, 1e-907_qp, 1e-906_qp, 1e-905_qp, &
        1e-904_qp, 1e-903_qp, 1e-902_qp, 1e-901_qp, 1e-900_qp, 1e-899_qp, &
        1e-898_qp, 1e-897_qp, 1e-896_qp, 1e-895_qp, 1e-894_qp, 1e-893_qp, &
        1e-892_qp, 1e-891_qp, 1e-890_qp, 1e-889_qp, 1e-888_qp, 1e-887_qp, &
        1e-886_qp, 1e-885_qp, 1e-884_qp, 1e-883_qp, 1e-882_qp, 1e-881_qp, &
        1e-880_qp, 1e-879_qp, 1e-878_qp, 1e-877_qp, 1e-876_qp, 1e-875_qp, &
        1e-874_qp, 1e-873_qp, 1e-872_qp, 1e-871_qp, 1e-870_qp, 1e-869_qp, &
        1e-868_qp, 1e-867_qp, 1e-866_qp, 1e-865_qp, 1e-864_qp, 1e-863_qp, &
        1e-862_qp, 1e-861_qp, 1e-860_qp, 1e-859_qp, 1e-858_qp, 1e-857_qp, &
        1e-856_qp, 1e-855_qp, 1e-854_qp, 1e-853_qp, 1e-852_qp, 1e-851_qp, &
        1e-850_qp, 1e-849_qp, 1e-848_qp, 1e-847_qp, 1e-846_qp, 1e-845_qp, &
        1e-844_qp, 1e-843_qp, 1e-842_qp, 1e-841_qp, 1e-840_qp, 1e-839_qp, &
        1e-838_qp, 1e-837_qp, 1e-836_qp, 1e-835_qp, 1e-834_qp, 1e-833_qp, &
        1e-832_qp, 1e-831_qp, 1e-830_qp, 1e-829_qp, 1e-828_qp, 1e-827_qp, &
        1e-826_qp, 1e-825_qp, 1e-824_qp, 1e-823_qp, 1e-822_qp, 1e-821_qp, &
        1e-820_qp, 1e-819_qp, 1e-818_qp, 1e-817_qp, 1e-816_qp, 1e-815_qp, &
        1e-814_qp, 1e-813_qp, 1e-812_qp, 1e-811_qp, 1e-810_qp, 1e-809_qp, &
        1e-808_qp, 1e-807_qp, 1e-806_qp, 1e-805_qp, 1e-804_qp, 1e-803_qp, &
        1e-802_qp, 1e-801_qp, 1e-800_qp, 1e-799_qp, 1e-798_qp, 1e-797_qp, &
        1e-796_qp, 1e-795_qp, 1e-794_qp, 1e-793_qp, 1e-792_qp, 1e-791_qp, &
        1e-790_qp, 1e-789_qp, 1e-788_qp, 1e-787_qp, 1e-786_qp, 1e-785_qp, &
        1e-784_qp, 1e-783_qp, 1e-782_qp, 1e-781_qp, 1e-780_qp, 1e-779_qp, &
        1e-778_qp, 1e-777_qp, 1e-776_qp, 1e-775_qp, 1e-774_qp, 1e-773_qp, &
        1e-772_qp, 1e-771_qp, 1e-770_qp, 1e-769_qp, 1e-768_qp, 1e-767_qp, &
        1e-766_qp, 1e-765_qp, 1e-764_qp, 1e-763_qp, 1e-762_qp, 1e-761_qp, &
        1e-760_qp, 1e-759_qp, 1e-758_qp, 1e-757_qp, 1e-756_qp, 1e-755_qp, &
        1e-754_qp, 1e-753_qp, 1e-752_qp, 1e-751_qp, 1e-750_qp, 1e-749_qp, &
        1e-748_qp, 1e-747_qp, 1e-746_qp, 1e-745_qp, 1e-744_qp, 1e-743_qp, &
        1e-742_qp, 1e-741_qp, 1e-740_qp, 1e-739_qp, 1e-738_qp, 1e-737_qp, &
        1e-736_qp, 1e-735_qp, 1e-734_qp, 1e-733_qp, 1e-732_qp, 1e-731_qp, &
        1e-730_qp, 1e-729_qp, 1e-728_qp, 1e-727_qp, 1e-726_qp, 1e-725_qp, &
        1e-724_qp, 1e-723_qp, 1e-722_qp, 1e-721_qp, 1e-720_qp, 1e-719_qp, &
        1e-718_qp, 1e-717_qp, 1e-716_qp, 1e-715_qp, 1e-714_qp, 1e-713_qp, &
        1e-712_qp, 1e-711_qp, 1e-710_qp, 1e-709_qp, 1e-708_qp, 1e-707_qp, &
        1e-706_qp, 1e-705_qp, 1e-704_qp, 1e-703_qp, 1e-702_qp, 1e-701_qp, &
        1e-700_qp, 1e-699_qp, 1e-698_qp, 1e-697_qp, 1e-696_qp, 1e-695_qp, &
        1e-694_qp, 1e-693_qp, 1e-692_qp, 1e-691_qp, 1e-690_qp, 1e-689_qp, &
        1e-688_qp, 1e-687_qp, 1e-686_qp, 1e-685_qp, 1e-684_qp, 1e-683_qp, &
        1e-682_qp, 1e-681_qp, 1e-680_qp, 1e-679_qp, 1e-678_qp, 1e-677_qp, &
        1e-676_qp, 1e-675_qp, 1e-674_qp, 1e-673_qp, 1e-672_qp, 1e-671_qp, &
        1e-670_qp, 1e-669_qp, 1e-668_qp, 1e-667_qp, 1e-666_qp, 1e-665_qp, &
        1e-664_qp, 1e-663_qp, 1e-662_qp, 1e-661_qp, 1e-660_qp, 1e-659_qp, &
        1e-658_qp, 1e-657_qp, 1e-656_qp, 1e-655_qp, 1e-654_qp, 1e-653_qp, &
        1e-652_qp, 1e-651_qp, 1e-650_qp, 1e-649_qp, 1e-648_qp, 1e-647_qp, &
        1e-646_qp, 1e-645_qp, 1e-644_qp, 1e-643_qp, 1e-642_qp, 1e-641_qp, &
        1e-640_qp, 1e-639_qp, 1e-638_qp, 1e-637_qp, 1e-636_qp, 1e-635_qp, &
        1e-634_qp, 1e-633_qp, 1e-632_qp, 1e-631_qp, 1e-630_qp, 1e-629_qp, &
        1e-628_qp, 1e-627_qp, 1e-626_qp, 1e-625_qp, 1e-624_qp, 1e-623_qp, &
        1e-622_qp, 1e-621_qp, 1e-620_qp, 1e-619_qp, 1e-618_qp, 1e-617_qp, &
        1e-616_qp, 1e-615_qp, 1e-614_qp, 1e-613_qp, 1e-612_qp, 1e-611_qp, &
        1e-610_qp, 1e-609_qp, 1e-608_qp, 1e-607_qp, 1e-606_qp, 1e-605_qp, &
        1e-604_qp, 1e-603_qp, 1e-602_qp, 1e-601_qp, 1e-600_qp, 1e-599_qp, &
        1e-598_qp, 1e-597_qp, 1e-596_qp, 1e-595_qp, 1e-594_qp, 1e-593_qp, &
        1e-592_qp, 1e-591_qp, 1e-590_qp, 1e-589_qp, 1e-588_qp, 1e-587_qp, &
        1e-586_qp, 1e-585_qp, 1e-584_qp, 1e-583_qp, 1e-582_qp, 1e-581_qp, &
        1e-580_qp, 1e-579_qp, 1e-578_qp, 1e-577_qp, 1e-576_qp, 1e-575_qp, &
        1e-574_qp, 1e-573_qp, 1e-572_qp, 1e-571_qp, 1e-570_qp, 1e-569_qp, &
        1e-568_qp, 1e-567_qp, 1e-566_qp, 1e-565_qp, 1e-564_qp, 1e-563_qp, &
        1e-562_qp, 1e-561_qp, 1e-560_qp, 1e-559_qp, 1e-558_qp, 1e-557_qp, &
        1e-556_qp, 1e-555_qp, 1e-554_qp, 1e-553_qp, 1e-552_qp, 1e-551_qp, &
        1e-550_qp, 1e-549_qp, 1e-548_qp, 1e-547_qp, 1e-546_qp, 1e-545_qp, &
        1e-544_qp, 1e-543_qp, 1e-542_qp, 1e-541_qp, 1e-540_qp, 1e-539_qp, &
        1e-538_qp, 1e-537_qp, 1e-536_qp, 1e-535_qp, 1e-534_qp, 1e-533_qp, &
        1e-532_qp, 1e-531_qp, 1e-530_qp, 1e-529_qp, 1e-528_qp, 1e-527_qp, &
        1e-526_qp, 1e-525_qp, 1e-524_qp, 1e-523_qp, 1e-522_qp, 1e-521_qp, &
        1e-520_qp, 1e-519_qp, 1e-518_qp, 1e-517_qp, 1e-516_qp, 1e-515_qp, &
        1e-514_qp, 1e-513_qp, 1e-512_qp, 1e-511_qp, 1e-510_qp, 1e-509_qp, &
        1e-508_qp, 1e-507_qp, 1e-506_qp, 1e-505_qp, 1e-504_qp, 1e-503_qp, &
        1e-502_qp, 1e-501_qp, 1e-500_qp, 1e-499_qp, 1e-498_qp, 1e-497_qp, &
        1e-496_qp, 1e-495_qp, 1e-494_qp, 1e-493_qp, 1e-492_qp, 1e-491_qp, &
        1e-490_qp, 1e-489_qp, 1e-488_qp, 1e-487_qp, 1e-486_qp, 1e-485_qp, &
        1e-484_qp, 1e-483_qp, 1e-482_qp, 1e-481_qp, 1e-480_qp, 1e-479_qp, &
        1e-478_qp, 1e-477_qp, 1e-476_qp, 1e-475_qp, 1e-474_qp, 1e-473_qp, &
        1e-472_qp, 1e-471_qp, 1e-470_qp, 1e-469_qp, 1e-468_qp, 1e-467_qp, &
        1e-466_qp, 1e-465_qp, 1e-464_qp, 1e-463_qp, 1e-462_qp, 1e-461_qp, &
        1e-460_qp, 1e-459_qp, 1e-458_qp, 1e-457_qp, 1e-456_qp, 1e-455_qp, &
        1e-454_qp, 1e-453_qp, 1e-452_qp, 1e-451_qp, 1e-450_qp, 1e-449_qp, &
        1e-448_qp, 1e-447_qp, 1e-446_qp, 1e-445_qp, 1e-444_qp, 1e-443_qp, &
        1e-442_qp, 1e-441_qp, 1e-440_qp, 1e-439_qp, 1e-438_qp, 1e-437_qp, &
        1e-436_qp, 1e-435_qp, 1e-434_qp, 1e-433_qp, 1e-432_qp, 1e-431_qp, &
        1e-430_qp, 1e-429_qp, 1e-428_qp, 1e-427_qp, 1e-426_qp, 1e-425_qp, &
        1e-424_qp, 1e-423_qp, 1e-422_qp, 1e-421_qp, 1e-420_qp, 1e-419_qp, &
        1e-418_qp, 1e-417_qp, 1e-416_qp, 1e-415_qp, 1e-414_qp, 1e-413_qp, &
        1e-412_qp, 1e-411_qp, 1e-410_qp, 1e-409_qp, 1e-408_qp, 1e-407_qp, &
        1e-406_qp, 1e-405_qp, 1e-404_qp, 1e-403_qp, 1e-402_qp, 1e-401_qp, &
        1e-400_qp, 1e-399_qp, 1e-398_qp, 1e-397_qp, 1e-396_qp, 1e-395_qp, &
        1e-394_qp, 1e-393_qp, 1e-392_qp, 1e-391_qp, 1e-390_qp, 1e-389_qp, &
        1e-388_qp, 1e-387_qp, 1e-386_qp, 1e-385_qp, 1e-384_qp, 1e-383_qp, &
        1e-382_qp, 1e-381_qp, 1e-380_qp, 1e-379_qp, 1e-378_qp, 1e-377_qp, &
        1e-376_qp, 1e-375_qp, 1e-374_qp, 1e-373_qp, 1e-372_qp, 1e-371_qp, &
        1e-370_qp, 1e-369_qp, 1e-368_qp, 1e-367_qp, 1e-366_qp, 1e-365_qp, &
        1e-364_qp, 1e-363_qp, 1e-362_qp, 1e-361_qp, 1e-360_qp, 1e-359_qp, &
        1e-358_qp, 1e-357_qp, 1e-356_qp, 1e-355_qp, 1e-354_qp, 1e-353_qp, &
        1e-352_qp, 1e-351_qp, 1e-350_qp, 1e-349_qp, 1e-348_qp, 1e-347_qp, &
        1e-346_qp, 1e-345_qp, 1e-344_qp, 1e-343_qp, 1e-342_qp, 1e-341_qp, &
        1e-340_qp, 1e-339_qp, 1e-338_qp, 1e-337_qp, 1e-336_qp, 1e-335_qp, &
        1e-334_qp, 1e-333_qp, 1e-332_qp, 1e-331_qp, 1e-330_qp, 1e-329_qp, &
        1e-328_qp, 1e-327_qp, 1e-326_qp, 1e-325_qp, 1e-324_qp, 1e-323_qp, &
        1e-322_qp, 1e-321_qp, 1e-320_qp, 1e-319_qp, 1e-318_qp, 1e-317_qp, &
        1e-316_qp, 1e-315_qp, 1e-314_qp, 1e-313_qp, 1e-312_qp, 1e-311_qp, &
        1e-310_qp, 1e-309_qp, 1e-308_qp, 1e-307_qp, 1e-306_qp, 1e-305_qp, &
        1e-304_qp, 1e-303_qp, 1e-302_qp, 1e-301_qp, 1e-300_qp, 1e-299_qp, &
        1e-298_qp, 1e-297_qp, 1e-296_qp, 1e-295_qp, 1e-294_qp, 1e-293_qp, &
        1e-292_qp, 1e-291_qp, 1e-290_qp, 1e-289_qp, 1e-288_qp, 1e-287_qp, &
        1e-286_qp, 1e-285_qp, 1e-284_qp, 1e-283_qp, 1e-282_qp, 1e-281_qp, &
        1e-280_qp, 1e-279_qp, 1e-278_qp, 1e-277_qp, 1e-276_qp, 1e-275_qp, &
        1e-274_qp, 1e-273_qp, 1e-272_qp, 1e-271_qp, 1e-270_qp, 1e-269_qp, &
        1e-268_qp, 1e-267_qp, 1e-266_qp, 1e-265_qp, 1e-264_qp, 1e-263_qp, &
        1e-262_qp, 1e-261_qp, 1e-260_qp, 1e-259_qp, 1e-258_qp, 1e-257_qp, &
        1e-256_qp, 1e-255_qp, 1e-254_qp, 1e-253_qp, 1e-252_qp, 1e-251_qp, &
        1e-250_qp, 1e-249_qp, 1e-248_qp, 1e-247_qp, 1e-246_qp, 1e-245_qp, &
        1e-244_qp, 1e-243_qp, 1e-242_qp, 1e-241_qp, 1e-240_qp, 1e-239_qp, &
        1e-238_qp, 1e-237_qp, 1e-236_qp, 1e-235_qp, 1e-234_qp, 1e-233_qp, &
        1e-232_qp, 1e-231_qp, 1e-230_qp, 1e-229_qp, 1e-228_qp, 1e-227_qp, &
        1e-226_qp, 1e-225_qp, 1e-224_qp, 1e-223_qp, 1e-222_qp, 1e-221_qp, &
        1e-220_qp, 1e-219_qp, 1e-218_qp, 1e-217_qp, 1e-216_qp, 1e-215_qp, &
        1e-214_qp, 1e-213_qp, 1e-212_qp, 1e-211_qp, 1e-210_qp, 1e-209_qp, &
        1e-208_qp, 1e-207_qp, 1e-206_qp, 1e-205_qp, 1e-204_qp, 1e-203_qp, &
        1e-202_qp, 1e-201_qp, 1e-200_qp, 1e-199_qp, 1e-198_qp, 1e-197_qp, &
        1e-196_qp, 1e-195_qp, 1e-194_qp, 1e-193_qp, 1e-192_qp, 1e-191_qp, &
        1e-190_qp, 1e-189_qp, 1e-188_qp, 1e-187_qp, 1e-186_qp, 1e-185_qp, &
        1e-184_qp, 1e-183_qp, 1e-182_qp, 1e-181_qp, 1e-180_qp, 1e-179_qp, &
        1e-178_qp, 1e-177_qp, 1e-176_qp, 1e-175_qp, 1e-174_qp, 1e-173_qp, &
        1e-172_qp, 1e-171_qp, 1e-170_qp, 1e-169_qp, 1e-168_qp, 1e-167_qp, &
        1e-166_qp, 1e-165_qp, 1e-164_qp, 1e-163_qp, 1e-162_qp, 1e-161_qp, &
        1e-160_qp, 1e-159_qp, 1e-158_qp, 1e-157_qp, 1e-156_qp, 1e-155_qp, &
        1e-154_qp, 1e-153_qp, 1e-152_qp, 1e-151_qp, 1e-150_qp, 1e-149_qp, &
        1e-148_qp, 1e-147_qp, 1e-146_qp, 1e-145_qp, 1e-144_qp, 1e-143_qp, &
        1e-142_qp, 1e-141_qp, 1e-140_qp, 1e-139_qp, 1e-138_qp, 1e-137_qp, &
        1e-136_qp, 1e-135_qp, 1e-134_qp, 1e-133_qp, 1e-132_qp, 1e-131_qp, &
        1e-130_qp, 1e-129_qp, 1e-128_qp, 1e-127_qp, 1e-126_qp, 1e-125_qp, &
        1e-124_qp, 1e-123_qp, 1e-122_qp, 1e-121_qp, 1e-120_qp, 1e-119_qp, &
        1e-118_qp, 1e-117_qp, 1e-116_qp, 1e-115_qp, 1e-114_qp, 1e-113_qp, &
        1e-112_qp, 1e-111_qp, 1e-110_qp, 1e-109_qp, 1e-108_qp, 1e-107_qp, &
        1e-106_qp, 1e-105_qp, 1e-104_qp, 1e-103_qp, 1e-102_qp, 1e-101_qp, &
        1e-100_qp, 1e-99_qp, 1e-98_qp, 1e-97_qp, 1e-96_qp, 1e-95_qp, &
        1e-94_qp, 1e-93_qp, 1e-92_qp, 1e-91_qp, 1e-90_qp, 1e-89_qp, 1e-88_qp, &
        1e-87_qp, 1e-86_qp, 1e-85_qp, 1e-84_qp, 1e-83_qp, 1e-82_qp, 1e-81_qp, &
        1e-80_qp, 1e-79_qp, 1e-78_qp, 1e-77_qp, 1e-76_qp, 1e-75_qp, 1e-74_qp, &
        1e-73_qp, 1e-72_qp, 1e-71_qp, 1e-70_qp, 1e-69_qp, 1e-68_qp, 1e-67_qp, &
        1e-66_qp, 1e-65_qp, 1e-64_qp, 1e-63_qp, 1e-62_qp, 1e-61_qp, 1e-60_qp, &
        1e-59_qp, 1e-58_qp, 1e-57_qp, 1e-56_qp, 1e-55_qp, 1e-54_qp, 1e-53_qp, &
        1e-52_qp, 1e-51_qp, 1e-50_qp, 1e-49_qp, 1e-48_qp, 1e-47_qp, 1e-46_qp, &
        1e-45_qp, 1e-44_qp, 1e-43_qp, 1e-42_qp, 1e-41_qp, 1e-40_qp, 1e-39_qp, &
        1e-38_qp, 1e-37_qp, 1e-36_qp, 1e-35_qp, 1e-34_qp, 1e-33_qp, 1e-32_qp, &
        1e-31_qp, 1e-30_qp, 1e-29_qp, 1e-28_qp, 1e-27_qp, 1e-26_qp, 1e-25_qp, &
        1e-24_qp, 1e-23_qp, 1e-22_qp, 1e-21_qp, 1e-20_qp, 1e-19_qp, 1e-18_qp, &
        1e-17_qp, 1e-16_qp, 1e-15_qp, 1e-14_qp, 1e-13_qp, 1e-12_qp, 1e-11_qp, &
        1e-10_qp, 1e-9_qp, 1e-8_qp, 1e-7_qp, 1e-6_qp, 1e-5_qp, 1e-4_qp, &
        1e-3_qp, 1e-2_qp, 1e-1_qp, 1e0_qp, 1e1_qp, 1e2_qp, 1e3_qp, 1e4_qp, &
        1e5_qp, 1e6_qp, 1e7_qp, 1e8_qp, 1e9_qp, 1e10_qp, 1e11_qp, 1e12_qp, &
        1e13_qp, 1e14_qp, 1e15_qp, 1e16_qp, 1e17_qp, 1e18_qp, 1e19_qp, &
        1e20_qp, 1e21_qp, 1e22_qp, 1e23_qp, 1e24_qp, 1e25_qp, 1e26_qp, &
        1e27_qp, 1e28_qp, 1e29_qp, 1e30_qp, 1e31_qp, 1e32_qp, 1e33_qp, &
        1e34_qp, 1e35_qp, 1e36_qp, 1e37_qp, 1e38_qp, 1e39_qp, 1e40_qp, &
        1e41_qp, 1e42_qp, 1e43_qp, 1e44_qp, 1e45_qp, 1e46_qp, 1e47_qp, &
        1e48_qp, 1e49_qp, 1e50_qp, 1e51_qp, 1e52_qp, 1e53_qp, 1e54_qp, &
        1e55_qp, 1e56_qp, 1e57_qp, 1e58_qp, 1e59_qp, 1e60_qp, 1e61_qp, &
        1e62_qp, 1e63_qp, 1e64_qp, 1e65_qp, 1e66_qp, 1e67_qp, 1e68_qp, &
        1e69_qp, 1e70_qp, 1e71_qp, 1e72_qp, 1e73_qp, 1e74_qp, 1e75_qp, &
        1e76_qp, 1e77_qp, 1e78_qp, 1e79_qp, 1e80_qp, 1e81_qp, 1e82_qp, &
        1e83_qp, 1e84_qp, 1e85_qp, 1e86_qp, 1e87_qp, 1e88_qp, 1e89_qp, &
        1e90_qp, 1e91_qp, 1e92_qp, 1e93_qp, 1e94_qp, 1e95_qp, 1e96_qp, &
        1e97_qp, 1e98_qp, 1e99_qp, 1e100_qp, 1e101_qp, 1e102_qp, 1e103_qp, &
        1e104_qp, 1e105_qp, 1e106_qp, 1e107_qp, 1e108_qp, 1e109_qp, 1e110_qp, &
        1e111_qp, 1e112_qp, 1e113_qp, 1e114_qp, 1e115_qp, 1e116_qp, 1e117_qp, &
        1e118_qp, 1e119_qp, 1e120_qp, 1e121_qp, 1e122_qp, 1e123_qp, 1e124_qp, &
        1e125_qp, 1e126_qp, 1e127_qp, 1e128_qp, 1e129_qp, 1e130_qp, 1e131_qp, &
        1e132_qp, 1e133_qp, 1e134_qp, 1e135_qp, 1e136_qp, 1e137_qp, 1e138_qp, &
        1e139_qp, 1e140_qp, 1e141_qp, 1e142_qp, 1e143_qp, 1e144_qp, 1e145_qp, &
        1e146_qp, 1e147_qp, 1e148_qp, 1e149_qp, 1e150_qp, 1e151_qp, 1e152_qp, &
        1e153_qp, 1e154_qp, 1e155_qp, 1e156_qp, 1e157_qp, 1e158_qp, 1e159_qp, &
        1e160_qp, 1e161_qp, 1e162_qp, 1e163_qp, 1e164_qp, 1e165_qp, 1e166_qp, &
        1e167_qp, 1e168_qp, 1e169_qp, 1e170_qp, 1e171_qp, 1e172_qp, 1e173_qp, &
        1e174_qp, 1e175_qp, 1e176_qp, 1e177_qp, 1e178_qp, 1e179_qp, 1e180_qp, &
        1e181_qp, 1e182_qp, 1e183_qp, 1e184_qp, 1e185_qp, 1e186_qp, 1e187_qp, &
        1e188_qp, 1e189_qp, 1e190_qp, 1e191_qp, 1e192_qp, 1e193_qp, 1e194_qp, &
        1e195_qp, 1e196_qp, 1e197_qp, 1e198_qp, 1e199_qp, 1e200_qp, 1e201_qp, &
        1e202_qp, 1e203_qp, 1e204_qp, 1e205_qp, 1e206_qp, 1e207_qp, 1e208_qp, &
        1e209_qp, 1e210_qp, 1e211_qp, 1e212_qp, 1e213_qp, 1e214_qp, 1e215_qp, &
        1e216_qp, 1e217_qp, 1e218_qp, 1e219_qp, 1e220_qp, 1e221_qp, 1e222_qp, &
        1e223_qp, 1e224_qp, 1e225_qp, 1e226_qp, 1e227_qp, 1e228_qp, 1e229_qp, &
        1e230_qp, 1e231_qp, 1e232_qp, 1e233_qp, 1e234_qp, 1e235_qp, 1e236_qp, &
        1e237_qp, 1e238_qp, 1e239_qp, 1e240_qp, 1e241_qp, 1e242_qp, 1e243_qp, &
        1e244_qp, 1e245_qp, 1e246_qp, 1e247_qp, 1e248_qp, 1e249_qp, 1e250_qp, &
        1e251_qp, 1e252_qp, 1e253_qp, 1e254_qp, 1e255_qp, 1e256_qp, 1e257_qp, &
        1e258_qp, 1e259_qp, 1e260_qp, 1e261_qp, 1e262_qp, 1e263_qp, 1e264_qp, &
        1e265_qp, 1e266_qp, 1e267_qp, 1e268_qp, 1e269_qp, 1e270_qp, 1e271_qp, &
        1e272_qp, 1e273_qp, 1e274_qp, 1e275_qp, 1e276_qp, 1e277_qp, 1e278_qp, &
        1e279_qp, 1e280_qp, 1e281_qp, 1e282_qp, 1e283_qp, 1e284_qp, 1e285_qp, &
        1e286_qp, 1e287_qp, 1e288_qp, 1e289_qp, 1e290_qp, 1e291_qp, 1e292_qp, &
        1e293_qp, 1e294_qp, 1e295_qp, 1e296_qp, 1e297_qp, 1e298_qp, 1e299_qp, &
        1e300_qp, 1e301_qp, 1e302_qp, 1e303_qp, 1e304_qp, 1e305_qp, 1e306_qp, &
        1e307_qp, 1e308_qp, 1e309_qp, 1e310_qp, 1e311_qp, 1e312_qp, 1e313_qp, &
        1e314_qp, 1e315_qp, 1e316_qp, 1e317_qp, 1e318_qp, 1e319_qp, 1e320_qp, &
        1e321_qp, 1e322_qp, 1e323_qp, 1e324_qp, 1e325_qp, 1e326_qp, 1e327_qp, &
        1e328_qp, 1e329_qp, 1e330_qp, 1e331_qp, 1e332_qp, 1e333_qp, 1e334_qp, &
        1e335_qp, 1e336_qp, 1e337_qp, 1e338_qp, 1e339_qp, 1e340_qp, 1e341_qp, &
        1e342_qp, 1e343_qp, 1e344_qp, 1e345_qp, 1e346_qp, 1e347_qp, 1e348_qp, &
        1e349_qp, 1e350_qp, 1e351_qp, 1e352_qp, 1e353_qp, 1e354_qp, 1e355_qp, &
        1e356_qp, 1e357_qp, 1e358_qp, 1e359_qp, 1e360_qp, 1e361_qp, 1e362_qp, &
        1e363_qp, 1e364_qp, 1e365_qp, 1e366_qp, 1e367_qp, 1e368_qp, 1e369_qp, &
        1e370_qp, 1e371_qp, 1e372_qp, 1e373_qp, 1e374_qp, 1e375_qp, 1e376_qp, &
        1e377_qp, 1e378_qp, 1e379_qp, 1e380_qp, 1e381_qp, 1e382_qp, 1e383_qp, &
        1e384_qp, 1e385_qp, 1e386_qp, 1e387_qp, 1e388_qp, 1e389_qp, 1e390_qp, &
        1e391_qp, 1e392_qp, 1e393_qp, 1e394_qp, 1e395_qp, 1e396_qp, 1e397_qp, &
        1e398_qp, 1e399_qp, 1e400_qp, 1e401_qp, 1e402_qp, 1e403_qp, 1e404_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS5(*) = [ &
        1e405_qp, 1e406_qp, 1e407_qp, 1e408_qp, 1e409_qp, 1e410_qp, 1e411_qp, &
        1e412_qp, 1e413_qp, 1e414_qp, 1e415_qp, 1e416_qp, 1e417_qp, 1e418_qp, &
        1e419_qp, 1e420_qp, 1e421_qp, 1e422_qp, 1e423_qp, 1e424_qp, 1e425_qp, &
        1e426_qp, 1e427_qp, 1e428_qp, 1e429_qp, 1e430_qp, 1e431_qp, 1e432_qp, &
        1e433_qp, 1e434_qp, 1e435_qp, 1e436_qp, 1e437_qp, 1e438_qp, 1e439_qp, &
        1e440_qp, 1e441_qp, 1e442_qp, 1e443_qp, 1e444_qp, 1e445_qp, 1e446_qp, &
        1e447_qp, 1e448_qp, 1e449_qp, 1e450_qp, 1e451_qp, 1e452_qp, 1e453_qp, &
        1e454_qp, 1e455_qp, 1e456_qp, 1e457_qp, 1e458_qp, 1e459_qp, 1e460_qp, &
        1e461_qp, 1e462_qp, 1e463_qp, 1e464_qp, 1e465_qp, 1e466_qp, 1e467_qp, &
        1e468_qp, 1e469_qp, 1e470_qp, 1e471_qp, 1e472_qp, 1e473_qp, 1e474_qp, &
        1e475_qp, 1e476_qp, 1e477_qp, 1e478_qp, 1e479_qp, 1e480_qp, 1e481_qp, &
        1e482_qp, 1e483_qp, 1e484_qp, 1e485_qp, 1e486_qp, 1e487_qp, 1e488_qp, &
        1e489_qp, 1e490_qp, 1e491_qp, 1e492_qp, 1e493_qp, 1e494_qp, 1e495_qp, &
        1e496_qp, 1e497_qp, 1e498_qp, 1e499_qp, 1e500_qp, 1e501_qp, 1e502_qp, &
        1e503_qp, 1e504_qp, 1e505_qp, 1e506_qp, 1e507_qp, 1e508_qp, 1e509_qp, &
        1e510_qp, 1e511_qp, 1e512_qp, 1e513_qp, 1e514_qp, 1e515_qp, 1e516_qp, &
        1e517_qp, 1e518_qp, 1e519_qp, 1e520_qp, 1e521_qp, 1e522_qp, 1e523_qp, &
        1e524_qp, 1e525_qp, 1e526_qp, 1e527_qp, 1e528_qp, 1e529_qp, 1e530_qp, &
        1e531_qp, 1e532_qp, 1e533_qp, 1e534_qp, 1e535_qp, 1e536_qp, 1e537_qp, &
        1e538_qp, 1e539_qp, 1e540_qp, 1e541_qp, 1e542_qp, 1e543_qp, 1e544_qp, &
        1e545_qp, 1e546_qp, 1e547_qp, 1e548_qp, 1e549_qp, 1e550_qp, 1e551_qp, &
        1e552_qp, 1e553_qp, 1e554_qp, 1e555_qp, 1e556_qp, 1e557_qp, 1e558_qp, &
        1e559_qp, 1e560_qp, 1e561_qp, 1e562_qp, 1e563_qp, 1e564_qp, 1e565_qp, &
        1e566_qp, 1e567_qp, 1e568_qp, 1e569_qp, 1e570_qp, 1e571_qp, 1e572_qp, &
        1e573_qp, 1e574_qp, 1e575_qp, 1e576_qp, 1e577_qp, 1e578_qp, 1e579_qp, &
        1e580_qp, 1e581_qp, 1e582_qp, 1e583_qp, 1e584_qp, 1e585_qp, 1e586_qp, &
        1e587_qp, 1e588_qp, 1e589_qp, 1e590_qp, 1e591_qp, 1e592_qp, 1e593_qp, &
        1e594_qp, 1e595_qp, 1e596_qp, 1e597_qp, 1e598_qp, 1e599_qp, 1e600_qp, &
        1e601_qp, 1e602_qp, 1e603_qp, 1e604_qp, 1e605_qp, 1e606_qp, 1e607_qp, &
        1e608_qp, 1e609_qp, 1e610_qp, 1e611_qp, 1e612_qp, 1e613_qp, 1e614_qp, &
        1e615_qp, 1e616_qp, 1e617_qp, 1e618_qp, 1e619_qp, 1e620_qp, 1e621_qp, &
        1e622_qp, 1e623_qp, 1e624_qp, 1e625_qp, 1e626_qp, 1e627_qp, 1e628_qp, &
        1e629_qp, 1e630_qp, 1e631_qp, 1e632_qp, 1e633_qp, 1e634_qp, 1e635_qp, &
        1e636_qp, 1e637_qp, 1e638_qp, 1e639_qp, 1e640_qp, 1e641_qp, 1e642_qp, &
        1e643_qp, 1e644_qp, 1e645_qp, 1e646_qp, 1e647_qp, 1e648_qp, 1e649_qp, &
        1e650_qp, 1e651_qp, 1e652_qp, 1e653_qp, 1e654_qp, 1e655_qp, 1e656_qp, &
        1e657_qp, 1e658_qp, 1e659_qp, 1e660_qp, 1e661_qp, 1e662_qp, 1e663_qp, &
        1e664_qp, 1e665_qp, 1e666_qp, 1e667_qp, 1e668_qp, 1e669_qp, 1e670_qp, &
        1e671_qp, 1e672_qp, 1e673_qp, 1e674_qp, 1e675_qp, 1e676_qp, 1e677_qp, &
        1e678_qp, 1e679_qp, 1e680_qp, 1e681_qp, 1e682_qp, 1e683_qp, 1e684_qp, &
        1e685_qp, 1e686_qp, 1e687_qp, 1e688_qp, 1e689_qp, 1e690_qp, 1e691_qp, &
        1e692_qp, 1e693_qp, 1e694_qp, 1e695_qp, 1e696_qp, 1e697_qp, 1e698_qp, &
        1e699_qp, 1e700_qp, 1e701_qp, 1e702_qp, 1e703_qp, 1e704_qp, 1e705_qp, &
        1e706_qp, 1e707_qp, 1e708_qp, 1e709_qp, 1e710_qp, 1e711_qp, 1e712_qp, &
        1e713_qp, 1e714_qp, 1e715_qp, 1e716_qp, 1e717_qp, 1e718_qp, 1e719_qp, &
        1e720_qp, 1e721_qp, 1e722_qp, 1e723_qp, 1e724_qp, 1e725_qp, 1e726_qp, &
        1e727_qp, 1e728_qp, 1e729_qp, 1e730_qp, 1e731_qp, 1e732_qp, 1e733_qp, &
        1e734_qp, 1e735_qp, 1e736_qp, 1e737_qp, 1e738_qp, 1e739_qp, 1e740_qp, &
        1e741_qp, 1e742_qp, 1e743_qp, 1e744_qp, 1e745_qp, 1e746_qp, 1e747_qp, &
        1e748_qp, 1e749_qp, 1e750_qp, 1e751_qp, 1e752_qp, 1e753_qp, 1e754_qp, &
        1e755_qp, 1e756_qp, 1e757_qp, 1e758_qp, 1e759_qp, 1e760_qp, 1e761_qp, &
        1e762_qp, 1e763_qp, 1e764_qp, 1e765_qp, 1e766_qp, 1e767_qp, 1e768_qp, &
        1e769_qp, 1e770_qp, 1e771_qp, 1e772_qp, 1e773_qp, 1e774_qp, 1e775_qp, &
        1e776_qp, 1e777_qp, 1e778_qp, 1e779_qp, 1e780_qp, 1e781_qp, 1e782_qp, &
        1e783_qp, 1e784_qp, 1e785_qp, 1e786_qp, 1e787_qp, 1e788_qp, 1e789_qp, &
        1e790_qp, 1e791_qp, 1e792_qp, 1e793_qp, 1e794_qp, 1e795_qp, 1e796_qp, &
        1e797_qp, 1e798_qp, 1e799_qp, 1e800_qp, 1e801_qp, 1e802_qp, 1e803_qp, &
        1e804_qp, 1e805_qp, 1e806_qp, 1e807_qp, 1e808_qp, 1e809_qp, 1e810_qp, &
        1e811_qp, 1e812_qp, 1e813_qp, 1e814_qp, 1e815_qp, 1e816_qp, 1e817_qp, &
        1e818_qp, 1e819_qp, 1e820_qp, 1e821_qp, 1e822_qp, 1e823_qp, 1e824_qp, &
        1e825_qp, 1e826_qp, 1e827_qp, 1e828_qp, 1e829_qp, 1e830_qp, 1e831_qp, &
        1e832_qp, 1e833_qp, 1e834_qp, 1e835_qp, 1e836_qp, 1e837_qp, 1e838_qp, &
        1e839_qp, 1e840_qp, 1e841_qp, 1e842_qp, 1e843_qp, 1e844_qp, 1e845_qp, &
        1e846_qp, 1e847_qp, 1e848_qp, 1e849_qp, 1e850_qp, 1e851_qp, 1e852_qp, &
        1e853_qp, 1e854_qp, 1e855_qp, 1e856_qp, 1e857_qp, 1e858_qp, 1e859_qp, &
        1e860_qp, 1e861_qp, 1e862_qp, 1e863_qp, 1e864_qp, 1e865_qp, 1e866_qp, &
        1e867_qp, 1e868_qp, 1e869_qp, 1e870_qp, 1e871_qp, 1e872_qp, 1e873_qp, &
        1e874_qp, 1e875_qp, 1e876_qp, 1e877_qp, 1e878_qp, 1e879_qp, 1e880_qp, &
        1e881_qp, 1e882_qp, 1e883_qp, 1e884_qp, 1e885_qp, 1e886_qp, 1e887_qp, &
        1e888_qp, 1e889_qp, 1e890_qp, 1e891_qp, 1e892_qp, 1e893_qp, 1e894_qp, &
        1e895_qp, 1e896_qp, 1e897_qp, 1e898_qp, 1e899_qp, 1e900_qp, 1e901_qp, &
        1e902_qp, 1e903_qp, 1e904_qp, 1e905_qp, 1e906_qp, 1e907_qp, 1e908_qp, &
        1e909_qp, 1e910_qp, 1e911_qp, 1e912_qp, 1e913_qp, 1e914_qp, 1e915_qp, &
        1e916_qp, 1e917_qp, 1e918_qp, 1e919_qp, 1e920_qp, 1e921_qp, 1e922_qp, &
        1e923_qp, 1e924_qp, 1e925_qp, 1e926_qp, 1e927_qp, 1e928_qp, 1e929_qp, &
        1e930_qp, 1e931_qp, 1e932_qp, 1e933_qp, 1e934_qp, 1e935_qp, 1e936_qp, &
        1e937_qp, 1e938_qp, 1e939_qp, 1e940_qp, 1e941_qp, 1e942_qp, 1e943_qp, &
        1e944_qp, 1e945_qp, 1e946_qp, 1e947_qp, 1e948_qp, 1e949_qp, 1e950_qp, &
        1e951_qp, 1e952_qp, 1e953_qp, 1e954_qp, 1e955_qp, 1e956_qp, 1e957_qp, &
        1e958_qp, 1e959_qp, 1e960_qp, 1e961_qp, 1e962_qp, 1e963_qp, 1e964_qp, &
        1e965_qp, 1e966_qp, 1e967_qp, 1e968_qp, 1e969_qp, 1e970_qp, 1e971_qp, &
        1e972_qp, 1e973_qp, 1e974_qp, 1e975_qp, 1e976_qp, 1e977_qp, 1e978_qp, &
        1e979_qp, 1e980_qp, 1e981_qp, 1e982_qp, 1e983_qp, 1e984_qp, 1e985_qp, &
        1e986_qp, 1e987_qp, 1e988_qp, 1e989_qp, 1e990_qp, 1e991_qp, 1e992_qp, &
        1e993_qp, 1e994_qp, 1e995_qp, 1e996_qp, 1e997_qp, 1e998_qp, 1e999_qp, &
        1e1000_qp, 1e1001_qp, 1e1002_qp, 1e1003_qp, 1e1004_qp, 1e1005_qp, &
        1e1006_qp, 1e1007_qp, 1e1008_qp, 1e1009_qp, 1e1010_qp, 1e1011_qp, &
        1e1012_qp, 1e1013_qp, 1e1014_qp, 1e1015_qp, 1e1016_qp, 1e1017_qp, &
        1e1018_qp, 1e1019_qp, 1e1020_qp, 1e1021_qp, 1e1022_qp, 1e1023_qp, &
        1e1024_qp, 1e1025_qp, 1e1026_qp, 1e1027_qp, 1e1028_qp, 1e1029_qp, &
        1e1030_qp, 1e1031_qp, 1e1032_qp, 1e1033_qp, 1e1034_qp, 1e1035_qp, &
        1e1036_qp, 1e1037_qp, 1e1038_qp, 1e1039_qp, 1e1040_qp, 1e1041_qp, &
        1e1042_qp, 1e1043_qp, 1e1044_qp, 1e1045_qp, 1e1046_qp, 1e1047_qp, &
        1e1048_qp, 1e1049_qp, 1e1050_qp, 1e1051_qp, 1e1052_qp, 1e1053_qp, &
        1e1054_qp, 1e1055_qp, 1e1056_qp, 1e1057_qp, 1e1058_qp, 1e1059_qp, &
        1e1060_qp, 1e1061_qp, 1e1062_qp, 1e1063_qp, 1e1064_qp, 1e1065_qp, &
        1e1066_qp, 1e1067_qp, 1e1068_qp, 1e1069_qp, 1e1070_qp, 1e1071_qp, &
        1e1072_qp, 1e1073_qp, 1e1074_qp, 1e1075_qp, 1e1076_qp, 1e1077_qp, &
        1e1078_qp, 1e1079_qp, 1e1080_qp, 1e1081_qp, 1e1082_qp, 1e1083_qp, &
        1e1084_qp, 1e1085_qp, 1e1086_qp, 1e1087_qp, 1e1088_qp, 1e1089_qp, &
        1e1090_qp, 1e1091_qp, 1e1092_qp, 1e1093_qp, 1e1094_qp, 1e1095_qp, &
        1e1096_qp, 1e1097_qp, 1e1098_qp, 1e1099_qp, 1e1100_qp, 1e1101_qp, &
        1e1102_qp, 1e1103_qp, 1e1104_qp, 1e1105_qp, 1e1106_qp, 1e1107_qp, &
        1e1108_qp, 1e1109_qp, 1e1110_qp, 1e1111_qp, 1e1112_qp, 1e1113_qp, &
        1e1114_qp, 1e1115_qp, 1e1116_qp, 1e1117_qp, 1e1118_qp, 1e1119_qp, &
        1e1120_qp, 1e1121_qp, 1e1122_qp, 1e1123_qp, 1e1124_qp, 1e1125_qp, &
        1e1126_qp, 1e1127_qp, 1e1128_qp, 1e1129_qp, 1e1130_qp, 1e1131_qp, &
        1e1132_qp, 1e1133_qp, 1e1134_qp, 1e1135_qp, 1e1136_qp, 1e1137_qp, &
        1e1138_qp, 1e1139_qp, 1e1140_qp, 1e1141_qp, 1e1142_qp, 1e1143_qp, &
        1e1144_qp, 1e1145_qp, 1e1146_qp, 1e1147_qp, 1e1148_qp, 1e1149_qp, &
        1e1150_qp, 1e1151_qp, 1e1152_qp, 1e1153_qp, 1e1154_qp, 1e1155_qp, &
        1e1156_qp, 1e1157_qp, 1e1158_qp, 1e1159_qp, 1e1160_qp, 1e1161_qp, &
        1e1162_qp, 1e1163_qp, 1e1164_qp, 1e1165_qp, 1e1166_qp, 1e1167_qp, &
        1e1168_qp, 1e1169_qp, 1e1170_qp, 1e1171_qp, 1e1172_qp, 1e1173_qp, &
        1e1174_qp, 1e1175_qp, 1e1176_qp, 1e1177_qp, 1e1178_qp, 1e1179_qp, &
        1e1180_qp, 1e1181_qp, 1e1182_qp, 1e1183_qp, 1e1184_qp, 1e1185_qp, &
        1e1186_qp, 1e1187_qp, 1e1188_qp, 1e1189_qp, 1e1190_qp, 1e1191_qp, &
        1e1192_qp, 1e1193_qp, 1e1194_qp, 1e1195_qp, 1e1196_qp, 1e1197_qp, &
        1e1198_qp, 1e1199_qp, 1e1200_qp, 1e1201_qp, 1e1202_qp, 1e1203_qp, &
        1e1204_qp, 1e1205_qp, 1e1206_qp, 1e1207_qp, 1e1208_qp, 1e1209_qp, &
        1e1210_qp, 1e1211_qp, 1e1212_qp, 1e1213_qp, 1e1214_qp, 1e1215_qp, &
        1e1216_qp, 1e1217_qp, 1e1218_qp, 1e1219_qp, 1e1220_qp, 1e1221_qp, &
        1e1222_qp, 1e1223_qp, 1e1224_qp, 1e1225_qp, 1e1226_qp, 1e1227_qp, &
        1e1228_qp, 1e1229_qp, 1e1230_qp, 1e1231_qp, 1e1232_qp, 1e1233_qp, &
        1e1234_qp, 1e1235_qp, 1e1236_qp, 1e1237_qp, 1e1238_qp, 1e1239_qp, &
        1e1240_qp, 1e1241_qp, 1e1242_qp, 1e1243_qp, 1e1244_qp, 1e1245_qp, &
        1e1246_qp, 1e1247_qp, 1e1248_qp, 1e1249_qp, 1e1250_qp, 1e1251_qp, &
        1e1252_qp, 1e1253_qp, 1e1254_qp, 1e1255_qp, 1e1256_qp, 1e1257_qp, &
        1e1258_qp, 1e1259_qp, 1e1260_qp, 1e1261_qp, 1e1262_qp, 1e1263_qp, &
        1e1264_qp, 1e1265_qp, 1e1266_qp, 1e1267_qp, 1e1268_qp, 1e1269_qp, &
        1e1270_qp, 1e1271_qp, 1e1272_qp, 1e1273_qp, 1e1274_qp, 1e1275_qp, &
        1e1276_qp, 1e1277_qp, 1e1278_qp, 1e1279_qp, 1e1280_qp, 1e1281_qp, &
        1e1282_qp, 1e1283_qp, 1e1284_qp, 1e1285_qp, 1e1286_qp, 1e1287_qp, &
        1e1288_qp, 1e1289_qp, 1e1290_qp, 1e1291_qp, 1e1292_qp, 1e1293_qp, &
        1e1294_qp, 1e1295_qp, 1e1296_qp, 1e1297_qp, 1e1298_qp, 1e1299_qp, &
        1e1300_qp, 1e1301_qp, 1e1302_qp, 1e1303_qp, 1e1304_qp, 1e1305_qp, &
        1e1306_qp, 1e1307_qp, 1e1308_qp, 1e1309_qp, 1e1310_qp, 1e1311_qp, &
        1e1312_qp, 1e1313_qp, 1e1314_qp, 1e1315_qp, 1e1316_qp, 1e1317_qp, &
        1e1318_qp, 1e1319_qp, 1e1320_qp, 1e1321_qp, 1e1322_qp, 1e1323_qp, &
        1e1324_qp, 1e1325_qp, 1e1326_qp, 1e1327_qp, 1e1328_qp, 1e1329_qp, &
        1e1330_qp, 1e1331_qp, 1e1332_qp, 1e1333_qp, 1e1334_qp, 1e1335_qp, &
        1e1336_qp, 1e1337_qp, 1e1338_qp, 1e1339_qp, 1e1340_qp, 1e1341_qp, &
        1e1342_qp, 1e1343_qp, 1e1344_qp, 1e1345_qp, 1e1346_qp, 1e1347_qp, &
        1e1348_qp, 1e1349_qp, 1e1350_qp, 1e1351_qp, 1e1352_qp, 1e1353_qp, &
        1e1354_qp, 1e1355_qp, 1e1356_qp, 1e1357_qp, 1e1358_qp, 1e1359_qp, &
        1e1360_qp, 1e1361_qp, 1e1362_qp, 1e1363_qp, 1e1364_qp, 1e1365_qp, &
        1e1366_qp, 1e1367_qp, 1e1368_qp, 1e1369_qp, 1e1370_qp, 1e1371_qp, &
        1e1372_qp, 1e1373_qp, 1e1374_qp, 1e1375_qp, 1e1376_qp, 1e1377_qp, &
        1e1378_qp, 1e1379_qp, 1e1380_qp, 1e1381_qp, 1e1382_qp, 1e1383_qp, &
        1e1384_qp, 1e1385_qp, 1e1386_qp, 1e1387_qp, 1e1388_qp, 1e1389_qp, &
        1e1390_qp, 1e1391_qp, 1e1392_qp, 1e1393_qp, 1e1394_qp, 1e1395_qp, &
        1e1396_qp, 1e1397_qp, 1e1398_qp, 1e1399_qp, 1e1400_qp, 1e1401_qp, &
        1e1402_qp, 1e1403_qp, 1e1404_qp, 1e1405_qp, 1e1406_qp, 1e1407_qp, &
        1e1408_qp, 1e1409_qp, 1e1410_qp, 1e1411_qp, 1e1412_qp, 1e1413_qp, &
        1e1414_qp, 1e1415_qp, 1e1416_qp, 1e1417_qp, 1e1418_qp, 1e1419_qp, &
        1e1420_qp, 1e1421_qp, 1e1422_qp, 1e1423_qp, 1e1424_qp, 1e1425_qp, &
        1e1426_qp, 1e1427_qp, 1e1428_qp, 1e1429_qp, 1e1430_qp, 1e1431_qp, &
        1e1432_qp, 1e1433_qp, 1e1434_qp, 1e1435_qp, 1e1436_qp, 1e1437_qp, &
        1e1438_qp, 1e1439_qp, 1e1440_qp, 1e1441_qp, 1e1442_qp, 1e1443_qp, &
        1e1444_qp, 1e1445_qp, 1e1446_qp, 1e1447_qp, 1e1448_qp, 1e1449_qp, &
        1e1450_qp, 1e1451_qp, 1e1452_qp, 1e1453_qp, 1e1454_qp, 1e1455_qp, &
        1e1456_qp, 1e1457_qp, 1e1458_qp, 1e1459_qp, 1e1460_qp, 1e1461_qp, &
        1e1462_qp, 1e1463_qp, 1e1464_qp, 1e1465_qp, 1e1466_qp, 1e1467_qp, &
        1e1468_qp, 1e1469_qp, 1e1470_qp, 1e1471_qp, 1e1472_qp, 1e1473_qp, &
        1e1474_qp, 1e1475_qp, 1e1476_qp, 1e1477_qp, 1e1478_qp, 1e1479_qp, &
        1e1480_qp, 1e1481_qp, 1e1482_qp, 1e1483_qp, 1e1484_qp, 1e1485_qp, &
        1e1486_qp, 1e1487_qp, 1e1488_qp, 1e1489_qp, 1e1490_qp, 1e1491_qp, &
        1e1492_qp, 1e1493_qp, 1e1494_qp, 1e1495_qp, 1e1496_qp, 1e1497_qp, &
        1e1498_qp, 1e1499_qp, 1e1500_qp, 1e1501_qp, 1e1502_qp, 1e1503_qp, &
        1e1504_qp, 1e1505_qp, 1e1506_qp, 1e1507_qp, 1e1508_qp, 1e1509_qp, &
        1e1510_qp, 1e1511_qp, 1e1512_qp, 1e1513_qp, 1e1514_qp, 1e1515_qp, &
        1e1516_qp, 1e1517_qp, 1e1518_qp, 1e1519_qp, 1e1520_qp, 1e1521_qp, &
        1e1522_qp, 1e1523_qp, 1e1524_qp, 1e1525_qp, 1e1526_qp, 1e1527_qp, &
        1e1528_qp, 1e1529_qp, 1e1530_qp, 1e1531_qp, 1e1532_qp, 1e1533_qp, &
        1e1534_qp, 1e1535_qp, 1e1536_qp, 1e1537_qp, 1e1538_qp, 1e1539_qp, &
        1e1540_qp, 1e1541_qp, 1e1542_qp, 1e1543_qp, 1e1544_qp, 1e1545_qp, &
        1e1546_qp, 1e1547_qp, 1e1548_qp, 1e1549_qp, 1e1550_qp, 1e1551_qp, &
        1e1552_qp, 1e1553_qp, 1e1554_qp, 1e1555_qp, 1e1556_qp, 1e1557_qp, &
        1e1558_qp, 1e1559_qp, 1e1560_qp, 1e1561_qp, 1e1562_qp, 1e1563_qp, &
        1e1564_qp, 1e1565_qp, 1e1566_qp, 1e1567_qp, 1e1568_qp, 1e1569_qp, &
        1e1570_qp, 1e1571_qp, 1e1572_qp, 1e1573_qp, 1e1574_qp, 1e1575_qp, &
        1e1576_qp, 1e1577_qp, 1e1578_qp, 1e1579_qp, 1e1580_qp, 1e1581_qp, &
        1e1582_qp, 1e1583_qp, 1e1584_qp, 1e1585_qp, 1e1586_qp, 1e1587_qp, &
        1e1588_qp, 1e1589_qp, 1e1590_qp, 1e1591_qp, 1e1592_qp, 1e1593_qp, &
        1e1594_qp, 1e1595_qp, 1e1596_qp, 1e1597_qp, 1e1598_qp, 1e1599_qp, &
        1e1600_qp, 1e1601_qp, 1e1602_qp, 1e1603_qp, 1e1604_qp, 1e1605_qp, &
        1e1606_qp, 1e1607_qp, 1e1608_qp, 1e1609_qp, 1e1610_qp, 1e1611_qp, &
        1e1612_qp, 1e1613_qp, 1e1614_qp, 1e1615_qp, 1e1616_qp, 1e1617_qp, &
        1e1618_qp, 1e1619_qp, 1e1620_qp, 1e1621_qp, 1e1622_qp, 1e1623_qp, &
        1e1624_qp, 1e1625_qp, 1e1626_qp, 1e1627_qp, 1e1628_qp, 1e1629_qp, &
        1e1630_qp, 1e1631_qp, 1e1632_qp, 1e1633_qp, 1e1634_qp, 1e1635_qp, &
        1e1636_qp, 1e1637_qp, 1e1638_qp, 1e1639_qp, 1e1640_qp, 1e1641_qp, &
        1e1642_qp, 1e1643_qp, 1e1644_qp, 1e1645_qp, 1e1646_qp, 1e1647_qp, &
        1e1648_qp, 1e1649_qp, 1e1650_qp, 1e1651_qp, 1e1652_qp, 1e1653_qp, &
        1e1654_qp, 1e1655_qp, 1e1656_qp, 1e1657_qp, 1e1658_qp, 1e1659_qp, &
        1e1660_qp, 1e1661_qp, 1e1662_qp, 1e1663_qp, 1e1664_qp, 1e1665_qp, &
        1e1666_qp, 1e1667_qp, 1e1668_qp, 1e1669_qp, 1e1670_qp, 1e1671_qp, &
        1e1672_qp, 1e1673_qp, 1e1674_qp, 1e1675_qp, 1e1676_qp, 1e1677_qp, &
        1e1678_qp, 1e1679_qp, 1e1680_qp, 1e1681_qp, 1e1682_qp, 1e1683_qp, &
        1e1684_qp, 1e1685_qp, 1e1686_qp, 1e1687_qp, 1e1688_qp, 1e1689_qp, &
        1e1690_qp, 1e1691_qp, 1e1692_qp, 1e1693_qp, 1e1694_qp, 1e1695_qp, &
        1e1696_qp, 1e1697_qp, 1e1698_qp, 1e1699_qp, 1e1700_qp, 1e1701_qp, &
        1e1702_qp, 1e1703_qp, 1e1704_qp, 1e1705_qp, 1e1706_qp, 1e1707_qp, &
        1e1708_qp, 1e1709_qp, 1e1710_qp, 1e1711_qp, 1e1712_qp, 1e1713_qp, &
        1e1714_qp, 1e1715_qp, 1e1716_qp, 1e1717_qp, 1e1718_qp, 1e1719_qp, &
        1e1720_qp, 1e1721_qp, 1e1722_qp, 1e1723_qp, 1e1724_qp, 1e1725_qp, &
        1e1726_qp, 1e1727_qp, 1e1728_qp, 1e1729_qp, 1e1730_qp, 1e1731_qp, &
        1e1732_qp, 1e1733_qp, 1e1734_qp, 1e1735_qp, 1e1736_qp, 1e1737_qp, &
        1e1738_qp, 1e1739_qp, 1e1740_qp, 1e1741_qp, 1e1742_qp, 1e1743_qp, &
        1e1744_qp, 1e1745_qp, 1e1746_qp, 1e1747_qp, 1e1748_qp, 1e1749_qp, &
        1e1750_qp, 1e1751_qp, 1e1752_qp, 1e1753_qp, 1e1754_qp, 1e1755_qp, &
        1e1756_qp, 1e1757_qp, 1e1758_qp, 1e1759_qp, 1e1760_qp, 1e1761_qp, &
        1e1762_qp, 1e1763_qp, 1e1764_qp, 1e1765_qp, 1e1766_qp, 1e1767_qp, &
        1e1768_qp, 1e1769_qp, 1e1770_qp, 1e1771_qp, 1e1772_qp, 1e1773_qp, &
        1e1774_qp, 1e1775_qp, 1e1776_qp, 1e1777_qp, 1e1778_qp, 1e1779_qp, &
        1e1780_qp, 1e1781_qp, 1e1782_qp, 1e1783_qp, 1e1784_qp, 1e1785_qp, &
        1e1786_qp, 1e1787_qp, 1e1788_qp, 1e1789_qp, 1e1790_qp, 1e1791_qp, &
        1e1792_qp, 1e1793_qp, 1e1794_qp, 1e1795_qp, 1e1796_qp, 1e1797_qp, &
        1e1798_qp, 1e1799_qp, 1e1800_qp, 1e1801_qp, 1e1802_qp, 1e1803_qp, &
        1e1804_qp, 1e1805_qp, 1e1806_qp, 1e1807_qp, 1e1808_qp, 1e1809_qp, &
        1e1810_qp, 1e1811_qp, 1e1812_qp, 1e1813_qp, 1e1814_qp, 1e1815_qp, &
        1e1816_qp, 1e1817_qp, 1e1818_qp, 1e1819_qp, 1e1820_qp, 1e1821_qp, &
        1e1822_qp, 1e1823_qp, 1e1824_qp, 1e1825_qp, 1e1826_qp, 1e1827_qp, &
        1e1828_qp, 1e1829_qp, 1e1830_qp, 1e1831_qp, 1e1832_qp, 1e1833_qp, &
        1e1834_qp, 1e1835_qp, 1e1836_qp, 1e1837_qp, 1e1838_qp, 1e1839_qp, &
        1e1840_qp, 1e1841_qp, 1e1842_qp, 1e1843_qp, 1e1844_qp, 1e1845_qp, &
        1e1846_qp, 1e1847_qp, 1e1848_qp, 1e1849_qp, 1e1850_qp, 1e1851_qp, &
        1e1852_qp, 1e1853_qp, 1e1854_qp, 1e1855_qp, 1e1856_qp, 1e1857_qp, &
        1e1858_qp, 1e1859_qp, 1e1860_qp, 1e1861_qp, 1e1862_qp, 1e1863_qp, &
        1e1864_qp, 1e1865_qp, 1e1866_qp, 1e1867_qp, 1e1868_qp, 1e1869_qp, &
        1e1870_qp, 1e1871_qp, 1e1872_qp, 1e1873_qp, 1e1874_qp, 1e1875_qp, &
        1e1876_qp, 1e1877_qp, 1e1878_qp, 1e1879_qp, 1e1880_qp, 1e1881_qp, &
        1e1882_qp, 1e1883_qp, 1e1884_qp, 1e1885_qp, 1e1886_qp, 1e1887_qp, &
        1e1888_qp, 1e1889_qp, 1e1890_qp, 1e1891_qp, 1e1892_qp, 1e1893_qp, &
        1e1894_qp, 1e1895_qp, 1e1896_qp, 1e1897_qp, 1e1898_qp, 1e1899_qp, &
        1e1900_qp, 1e1901_qp, 1e1902_qp, 1e1903_qp, 1e1904_qp, 1e1905_qp, &
        1e1906_qp, 1e1907_qp, 1e1908_qp, 1e1909_qp, 1e1910_qp, 1e1911_qp, &
        1e1912_qp, 1e1913_qp, 1e1914_qp, 1e1915_qp, 1e1916_qp, 1e1917_qp, &
        1e1918_qp, 1e1919_qp, 1e1920_qp, 1e1921_qp, 1e1922_qp, 1e1923_qp, &
        1e1924_qp, 1e1925_qp, 1e1926_qp, 1e1927_qp, 1e1928_qp, 1e1929_qp, &
        1e1930_qp, 1e1931_qp, 1e1932_qp, 1e1933_qp, 1e1934_qp, 1e1935_qp, &
        1e1936_qp, 1e1937_qp, 1e1938_qp, 1e1939_qp, 1e1940_qp, 1e1941_qp, &
        1e1942_qp, 1e1943_qp, 1e1944_qp, 1e1945_qp, 1e1946_qp, 1e1947_qp, &
        1e1948_qp, 1e1949_qp, 1e1950_qp, 1e1951_qp, 1e1952_qp, 1e1953_qp, &
        1e1954_qp, 1e1955_qp, 1e1956_qp, 1e1957_qp, 1e1958_qp, 1e1959_qp, &
        1e1960_qp, 1e1961_qp, 1e1962_qp, 1e1963_qp, 1e1964_qp, 1e1965_qp, &
        1e1966_qp, 1e1967_qp, 1e1968_qp, 1e1969_qp, 1e1970_qp, 1e1971_qp, &
        1e1972_qp, 1e1973_qp, 1e1974_qp, 1e1975_qp, 1e1976_qp, 1e1977_qp, &
        1e1978_qp, 1e1979_qp, 1e1980_qp, 1e1981_qp, 1e1982_qp, 1e1983_qp, &
        1e1984_qp, 1e1985_qp, 1e1986_qp, 1e1987_qp, 1e1988_qp, 1e1989_qp, &
        1e1990_qp, 1e1991_qp, 1e1992_qp, 1e1993_qp, 1e1994_qp, 1e1995_qp, &
        1e1996_qp, 1e1997_qp, 1e1998_qp, 1e1999_qp, 1e2000_qp, 1e2001_qp, &
        1e2002_qp, 1e2003_qp, 1e2004_qp, 1e2005_qp, 1e2006_qp, 1e2007_qp, &
        1e2008_qp, 1e2009_qp, 1e2010_qp, 1e2011_qp, 1e2012_qp, 1e2013_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS6(*) = [ &
        1e2014_qp, 1e2015_qp, 1e2016_qp, 1e2017_qp, 1e2018_qp, 1e2019_qp, &
        1e2020_qp, 1e2021_qp, 1e2022_qp, 1e2023_qp, 1e2024_qp, 1e2025_qp, &
        1e2026_qp, 1e2027_qp, 1e2028_qp, 1e2029_qp, 1e2030_qp, 1e2031_qp, &
        1e2032_qp, 1e2033_qp, 1e2034_qp, 1e2035_qp, 1e2036_qp, 1e2037_qp, &
        1e2038_qp, 1e2039_qp, 1e2040_qp, 1e2041_qp, 1e2042_qp, 1e2043_qp, &
        1e2044_qp, 1e2045_qp, 1e2046_qp, 1e2047_qp, 1e2048_qp, 1e2049_qp, &
        1e2050_qp, 1e2051_qp, 1e2052_qp, 1e2053_qp, 1e2054_qp, 1e2055_qp, &
        1e2056_qp, 1e2057_qp, 1e2058_qp, 1e2059_qp, 1e2060_qp, 1e2061_qp, &
        1e2062_qp, 1e2063_qp, 1e2064_qp, 1e2065_qp, 1e2066_qp, 1e2067_qp, &
        1e2068_qp, 1e2069_qp, 1e2070_qp, 1e2071_qp, 1e2072_qp, 1e2073_qp, &
        1e2074_qp, 1e2075_qp, 1e2076_qp, 1e2077_qp, 1e2078_qp, 1e2079_qp, &
        1e2080_qp, 1e2081_qp, 1e2082_qp, 1e2083_qp, 1e2084_qp, 1e2085_qp, &
        1e2086_qp, 1e2087_qp, 1e2088_qp, 1e2089_qp, 1e2090_qp, 1e2091_qp, &
        1e2092_qp, 1e2093_qp, 1e2094_qp, 1e2095_qp, 1e2096_qp, 1e2097_qp, &
        1e2098_qp, 1e2099_qp, 1e2100_qp, 1e2101_qp, 1e2102_qp, 1e2103_qp, &
        1e2104_qp, 1e2105_qp, 1e2106_qp, 1e2107_qp, 1e2108_qp, 1e2109_qp, &
        1e2110_qp, 1e2111_qp, 1e2112_qp, 1e2113_qp, 1e2114_qp, 1e2115_qp, &
        1e2116_qp, 1e2117_qp, 1e2118_qp, 1e2119_qp, 1e2120_qp, 1e2121_qp, &
        1e2122_qp, 1e2123_qp, 1e2124_qp, 1e2125_qp, 1e2126_qp, 1e2127_qp, &
        1e2128_qp, 1e2129_qp, 1e2130_qp, 1e2131_qp, 1e2132_qp, 1e2133_qp, &
        1e2134_qp, 1e2135_qp, 1e2136_qp, 1e2137_qp, 1e2138_qp, 1e2139_qp, &
        1e2140_qp, 1e2141_qp, 1e2142_qp, 1e2143_qp, 1e2144_qp, 1e2145_qp, &
        1e2146_qp, 1e2147_qp, 1e2148_qp, 1e2149_qp, 1e2150_qp, 1e2151_qp, &
        1e2152_qp, 1e2153_qp, 1e2154_qp, 1e2155_qp, 1e2156_qp, 1e2157_qp, &
        1e2158_qp, 1e2159_qp, 1e2160_qp, 1e2161_qp, 1e2162_qp, 1e2163_qp, &
        1e2164_qp, 1e2165_qp, 1e2166_qp, 1e2167_qp, 1e2168_qp, 1e2169_qp, &
        1e2170_qp, 1e2171_qp, 1e2172_qp, 1e2173_qp, 1e2174_qp, 1e2175_qp, &
        1e2176_qp, 1e2177_qp, 1e2178_qp, 1e2179_qp, 1e2180_qp, 1e2181_qp, &
        1e2182_qp, 1e2183_qp, 1e2184_qp, 1e2185_qp, 1e2186_qp, 1e2187_qp, &
        1e2188_qp, 1e2189_qp, 1e2190_qp, 1e2191_qp, 1e2192_qp, 1e2193_qp, &
        1e2194_qp, 1e2195_qp, 1e2196_qp, 1e2197_qp, 1e2198_qp, 1e2199_qp, &
        1e2200_qp, 1e2201_qp, 1e2202_qp, 1e2203_qp, 1e2204_qp, 1e2205_qp, &
        1e2206_qp, 1e2207_qp, 1e2208_qp, 1e2209_qp, 1e2210_qp, 1e2211_qp, &
        1e2212_qp, 1e2213_qp, 1e2214_qp, 1e2215_qp, 1e2216_qp, 1e2217_qp, &
        1e2218_qp, 1e2219_qp, 1e2220_qp, 1e2221_qp, 1e2222_qp, 1e2223_qp, &
        1e2224_qp, 1e2225_qp, 1e2226_qp, 1e2227_qp, 1e2228_qp, 1e2229_qp, &
        1e2230_qp, 1e2231_qp, 1e2232_qp, 1e2233_qp, 1e2234_qp, 1e2235_qp, &
        1e2236_qp, 1e2237_qp, 1e2238_qp, 1e2239_qp, 1e2240_qp, 1e2241_qp, &
        1e2242_qp, 1e2243_qp, 1e2244_qp, 1e2245_qp, 1e2246_qp, 1e2247_qp, &
        1e2248_qp, 1e2249_qp, 1e2250_qp, 1e2251_qp, 1e2252_qp, 1e2253_qp, &
        1e2254_qp, 1e2255_qp, 1e2256_qp, 1e2257_qp, 1e2258_qp, 1e2259_qp, &
        1e2260_qp, 1e2261_qp, 1e2262_qp, 1e2263_qp, 1e2264_qp, 1e2265_qp, &
        1e2266_qp, 1e2267_qp, 1e2268_qp, 1e2269_qp, 1e2270_qp, 1e2271_qp, &
        1e2272_qp, 1e2273_qp, 1e2274_qp, 1e2275_qp, 1e2276_qp, 1e2277_qp, &
        1e2278_qp, 1e2279_qp, 1e2280_qp, 1e2281_qp, 1e2282_qp, 1e2283_qp, &
        1e2284_qp, 1e2285_qp, 1e2286_qp, 1e2287_qp, 1e2288_qp, 1e2289_qp, &
        1e2290_qp, 1e2291_qp, 1e2292_qp, 1e2293_qp, 1e2294_qp, 1e2295_qp, &
        1e2296_qp, 1e2297_qp, 1e2298_qp, 1e2299_qp, 1e2300_qp, 1e2301_qp, &
        1e2302_qp, 1e2303_qp, 1e2304_qp, 1e2305_qp, 1e2306_qp, 1e2307_qp, &
        1e2308_qp, 1e2309_qp, 1e2310_qp, 1e2311_qp, 1e2312_qp, 1e2313_qp, &
        1e2314_qp, 1e2315_qp, 1e2316_qp, 1e2317_qp, 1e2318_qp, 1e2319_qp, &
        1e2320_qp, 1e2321_qp, 1e2322_qp, 1e2323_qp, 1e2324_qp, 1e2325_qp, &
        1e2326_qp, 1e2327_qp, 1e2328_qp, 1e2329_qp, 1e2330_qp, 1e2331_qp, &
        1e2332_qp, 1e2333_qp, 1e2334_qp, 1e2335_qp, 1e2336_qp, 1e2337_qp, &
        1e2338_qp, 1e2339_qp, 1e2340_qp, 1e2341_qp, 1e2342_qp, 1e2343_qp, &
        1e2344_qp, 1e2345_qp, 1e2346_qp, 1e2347_qp, 1e2348_qp, 1e2349_qp, &
        1e2350_qp, 1e2351_qp, 1e2352_qp, 1e2353_qp, 1e2354_qp, 1e2355_qp, &
        1e2356_qp, 1e2357_qp, 1e2358_qp, 1e2359_qp, 1e2360_qp, 1e2361_qp, &
        1e2362_qp, 1e2363_qp, 1e2364_qp, 1e2365_qp, 1e2366_qp, 1e2367_qp, &
        1e2368_qp, 1e2369_qp, 1e2370_qp, 1e2371_qp, 1e2372_qp, 1e2373_qp, &
        1e2374_qp, 1e2375_qp, 1e2376_qp, 1e2377_qp, 1e2378_qp, 1e2379_qp, &
        1e2380_qp, 1e2381_qp, 1e2382_qp, 1e2383_qp, 1e2384_qp, 1e2385_qp, &
        1e2386_qp, 1e2387_qp, 1e2388_qp, 1e2389_qp, 1e2390_qp, 1e2391_qp, &
        1e2392_qp, 1e2393_qp, 1e2394_qp, 1e2395_qp, 1e2396_qp, 1e2397_qp, &
        1e2398_qp, 1e2399_qp, 1e2400_qp, 1e2401_qp, 1e2402_qp, 1e2403_qp, &
        1e2404_qp, 1e2405_qp, 1e2406_qp, 1e2407_qp, 1e2408_qp, 1e2409_qp, &
        1e2410_qp, 1e2411_qp, 1e2412_qp, 1e2413_qp, 1e2414_qp, 1e2415_qp, &
        1e2416_qp, 1e2417_qp, 1e2418_qp, 1e2419_qp, 1e2420_qp, 1e2421_qp, &
        1e2422_qp, 1e2423_qp, 1e2424_qp, 1e2425_qp, 1e2426_qp, 1e2427_qp, &
        1e2428_qp, 1e2429_qp, 1e2430_qp, 1e2431_qp, 1e2432_qp, 1e2433_qp, &
        1e2434_qp, 1e2435_qp, 1e2436_qp, 1e2437_qp, 1e2438_qp, 1e2439_qp, &
        1e2440_qp, 1e2441_qp, 1e2442_qp, 1e2443_qp, 1e2444_qp, 1e2445_qp, &
        1e2446_qp, 1e2447_qp, 1e2448_qp, 1e2449_qp, 1e2450_qp, 1e2451_qp, &
        1e2452_qp, 1e2453_qp, 1e2454_qp, 1e2455_qp, 1e2456_qp, 1e2457_qp, &
        1e2458_qp, 1e2459_qp, 1e2460_qp, 1e2461_qp, 1e2462_qp, 1e2463_qp, &
        1e2464_qp, 1e2465_qp, 1e2466_qp, 1e2467_qp, 1e2468_qp, 1e2469_qp, &
        1e2470_qp, 1e2471_qp, 1e2472_qp, 1e2473_qp, 1e2474_qp, 1e2475_qp, &
        1e2476_qp, 1e2477_qp, 1e2478_qp, 1e2479_qp, 1e2480_qp, 1e2481_qp, &
        1e2482_qp, 1e2483_qp, 1e2484_qp, 1e2485_qp, 1e2486_qp, 1e2487_qp, &
        1e2488_qp, 1e2489_qp, 1e2490_qp, 1e2491_qp, 1e2492_qp, 1e2493_qp, &
        1e2494_qp, 1e2495_qp, 1e2496_qp, 1e2497_qp, 1e2498_qp, 1e2499_qp, &
        1e2500_qp, 1e2501_qp, 1e2502_qp, 1e2503_qp, 1e2504_qp, 1e2505_qp, &
        1e2506_qp, 1e2507_qp, 1e2508_qp, 1e2509_qp, 1e2510_qp, 1e2511_qp, &
        1e2512_qp, 1e2513_qp, 1e2514_qp, 1e2515_qp, 1e2516_qp, 1e2517_qp, &
        1e2518_qp, 1e2519_qp, 1e2520_qp, 1e2521_qp, 1e2522_qp, 1e2523_qp, &
        1e2524_qp, 1e2525_qp, 1e2526_qp, 1e2527_qp, 1e2528_qp, 1e2529_qp, &
        1e2530_qp, 1e2531_qp, 1e2532_qp, 1e2533_qp, 1e2534_qp, 1e2535_qp, &
        1e2536_qp, 1e2537_qp, 1e2538_qp, 1e2539_qp, 1e2540_qp, 1e2541_qp, &
        1e2542_qp, 1e2543_qp, 1e2544_qp, 1e2545_qp, 1e2546_qp, 1e2547_qp, &
        1e2548_qp, 1e2549_qp, 1e2550_qp, 1e2551_qp, 1e2552_qp, 1e2553_qp, &
        1e2554_qp, 1e2555_qp, 1e2556_qp, 1e2557_qp, 1e2558_qp, 1e2559_qp, &
        1e2560_qp, 1e2561_qp, 1e2562_qp, 1e2563_qp, 1e2564_qp, 1e2565_qp, &
        1e2566_qp, 1e2567_qp, 1e2568_qp, 1e2569_qp, 1e2570_qp, 1e2571_qp, &
        1e2572_qp, 1e2573_qp, 1e2574_qp, 1e2575_qp, 1e2576_qp, 1e2577_qp, &
        1e2578_qp, 1e2579_qp, 1e2580_qp, 1e2581_qp, 1e2582_qp, 1e2583_qp, &
        1e2584_qp, 1e2585_qp, 1e2586_qp, 1e2587_qp, 1e2588_qp, 1e2589_qp, &
        1e2590_qp, 1e2591_qp, 1e2592_qp, 1e2593_qp, 1e2594_qp, 1e2595_qp, &
        1e2596_qp, 1e2597_qp, 1e2598_qp, 1e2599_qp, 1e2600_qp, 1e2601_qp, &
        1e2602_qp, 1e2603_qp, 1e2604_qp, 1e2605_qp, 1e2606_qp, 1e2607_qp, &
        1e2608_qp, 1e2609_qp, 1e2610_qp, 1e2611_qp, 1e2612_qp, 1e2613_qp, &
        1e2614_qp, 1e2615_qp, 1e2616_qp, 1e2617_qp, 1e2618_qp, 1e2619_qp, &
        1e2620_qp, 1e2621_qp, 1e2622_qp, 1e2623_qp, 1e2624_qp, 1e2625_qp, &
        1e2626_qp, 1e2627_qp, 1e2628_qp, 1e2629_qp, 1e2630_qp, 1e2631_qp, &
        1e2632_qp, 1e2633_qp, 1e2634_qp, 1e2635_qp, 1e2636_qp, 1e2637_qp, &
        1e2638_qp, 1e2639_qp, 1e2640_qp, 1e2641_qp, 1e2642_qp, 1e2643_qp, &
        1e2644_qp, 1e2645_qp, 1e2646_qp, 1e2647_qp, 1e2648_qp, 1e2649_qp, &
        1e2650_qp, 1e2651_qp, 1e2652_qp, 1e2653_qp, 1e2654_qp, 1e2655_qp, &
        1e2656_qp, 1e2657_qp, 1e2658_qp, 1e2659_qp, 1e2660_qp, 1e2661_qp, &
        1e2662_qp, 1e2663_qp, 1e2664_qp, 1e2665_qp, 1e2666_qp, 1e2667_qp, &
        1e2668_qp, 1e2669_qp, 1e2670_qp, 1e2671_qp, 1e2672_qp, 1e2673_qp, &
        1e2674_qp, 1e2675_qp, 1e2676_qp, 1e2677_qp, 1e2678_qp, 1e2679_qp, &
        1e2680_qp, 1e2681_qp, 1e2682_qp, 1e2683_qp, 1e2684_qp, 1e2685_qp, &
        1e2686_qp, 1e2687_qp, 1e2688_qp, 1e2689_qp, 1e2690_qp, 1e2691_qp, &
        1e2692_qp, 1e2693_qp, 1e2694_qp, 1e2695_qp, 1e2696_qp, 1e2697_qp, &
        1e2698_qp, 1e2699_qp, 1e2700_qp, 1e2701_qp, 1e2702_qp, 1e2703_qp, &
        1e2704_qp, 1e2705_qp, 1e2706_qp, 1e2707_qp, 1e2708_qp, 1e2709_qp, &
        1e2710_qp, 1e2711_qp, 1e2712_qp, 1e2713_qp, 1e2714_qp, 1e2715_qp, &
        1e2716_qp, 1e2717_qp, 1e2718_qp, 1e2719_qp, 1e2720_qp, 1e2721_qp, &
        1e2722_qp, 1e2723_qp, 1e2724_qp, 1e2725_qp, 1e2726_qp, 1e2727_qp, &
        1e2728_qp, 1e2729_qp, 1e2730_qp, 1e2731_qp, 1e2732_qp, 1e2733_qp, &
        1e2734_qp, 1e2735_qp, 1e2736_qp, 1e2737_qp, 1e2738_qp, 1e2739_qp, &
        1e2740_qp, 1e2741_qp, 1e2742_qp, 1e2743_qp, 1e2744_qp, 1e2745_qp, &
        1e2746_qp, 1e2747_qp, 1e2748_qp, 1e2749_qp, 1e2750_qp, 1e2751_qp, &
        1e2752_qp, 1e2753_qp, 1e2754_qp, 1e2755_qp, 1e2756_qp, 1e2757_qp, &
        1e2758_qp, 1e2759_qp, 1e2760_qp, 1e2761_qp, 1e2762_qp, 1e2763_qp, &
        1e2764_qp, 1e2765_qp, 1e2766_qp, 1e2767_qp, 1e2768_qp, 1e2769_qp, &
        1e2770_qp, 1e2771_qp, 1e2772_qp, 1e2773_qp, 1e2774_qp, 1e2775_qp, &
        1e2776_qp, 1e2777_qp, 1e2778_qp, 1e2779_qp, 1e2780_qp, 1e2781_qp, &
        1e2782_qp, 1e2783_qp, 1e2784_qp, 1e2785_qp, 1e2786_qp, 1e2787_qp, &
        1e2788_qp, 1e2789_qp, 1e2790_qp, 1e2791_qp, 1e2792_qp, 1e2793_qp, &
        1e2794_qp, 1e2795_qp, 1e2796_qp, 1e2797_qp, 1e2798_qp, 1e2799_qp, &
        1e2800_qp, 1e2801_qp, 1e2802_qp, 1e2803_qp, 1e2804_qp, 1e2805_qp, &
        1e2806_qp, 1e2807_qp, 1e2808_qp, 1e2809_qp, 1e2810_qp, 1e2811_qp, &
        1e2812_qp, 1e2813_qp, 1e2814_qp, 1e2815_qp, 1e2816_qp, 1e2817_qp, &
        1e2818_qp, 1e2819_qp, 1e2820_qp, 1e2821_qp, 1e2822_qp, 1e2823_qp, &
        1e2824_qp, 1e2825_qp, 1e2826_qp, 1e2827_qp, 1e2828_qp, 1e2829_qp, &
        1e2830_qp, 1e2831_qp, 1e2832_qp, 1e2833_qp, 1e2834_qp, 1e2835_qp, &
        1e2836_qp, 1e2837_qp, 1e2838_qp, 1e2839_qp, 1e2840_qp, 1e2841_qp, &
        1e2842_qp, 1e2843_qp, 1e2844_qp, 1e2845_qp, 1e2846_qp, 1e2847_qp, &
        1e2848_qp, 1e2849_qp, 1e2850_qp, 1e2851_qp, 1e2852_qp, 1e2853_qp, &
        1e2854_qp, 1e2855_qp, 1e2856_qp, 1e2857_qp, 1e2858_qp, 1e2859_qp, &
        1e2860_qp, 1e2861_qp, 1e2862_qp, 1e2863_qp, 1e2864_qp, 1e2865_qp, &
        1e2866_qp, 1e2867_qp, 1e2868_qp, 1e2869_qp, 1e2870_qp, 1e2871_qp, &
        1e2872_qp, 1e2873_qp, 1e2874_qp, 1e2875_qp, 1e2876_qp, 1e2877_qp, &
        1e2878_qp, 1e2879_qp, 1e2880_qp, 1e2881_qp, 1e2882_qp, 1e2883_qp, &
        1e2884_qp, 1e2885_qp, 1e2886_qp, 1e2887_qp, 1e2888_qp, 1e2889_qp, &
        1e2890_qp, 1e2891_qp, 1e2892_qp, 1e2893_qp, 1e2894_qp, 1e2895_qp, &
        1e2896_qp, 1e2897_qp, 1e2898_qp, 1e2899_qp, 1e2900_qp, 1e2901_qp, &
        1e2902_qp, 1e2903_qp, 1e2904_qp, 1e2905_qp, 1e2906_qp, 1e2907_qp, &
        1e2908_qp, 1e2909_qp, 1e2910_qp, 1e2911_qp, 1e2912_qp, 1e2913_qp, &
        1e2914_qp, 1e2915_qp, 1e2916_qp, 1e2917_qp, 1e2918_qp, 1e2919_qp, &
        1e2920_qp, 1e2921_qp, 1e2922_qp, 1e2923_qp, 1e2924_qp, 1e2925_qp, &
        1e2926_qp, 1e2927_qp, 1e2928_qp, 1e2929_qp, 1e2930_qp, 1e2931_qp, &
        1e2932_qp, 1e2933_qp, 1e2934_qp, 1e2935_qp, 1e2936_qp, 1e2937_qp, &
        1e2938_qp, 1e2939_qp, 1e2940_qp, 1e2941_qp, 1e2942_qp, 1e2943_qp, &
        1e2944_qp, 1e2945_qp, 1e2946_qp, 1e2947_qp, 1e2948_qp, 1e2949_qp, &
        1e2950_qp, 1e2951_qp, 1e2952_qp, 1e2953_qp, 1e2954_qp, 1e2955_qp, &
        1e2956_qp, 1e2957_qp, 1e2958_qp, 1e2959_qp, 1e2960_qp, 1e2961_qp, &
        1e2962_qp, 1e2963_qp, 1e2964_qp, 1e2965_qp, 1e2966_qp, 1e2967_qp, &
        1e2968_qp, 1e2969_qp, 1e2970_qp, 1e2971_qp, 1e2972_qp, 1e2973_qp, &
        1e2974_qp, 1e2975_qp, 1e2976_qp, 1e2977_qp, 1e2978_qp, 1e2979_qp, &
        1e2980_qp, 1e2981_qp, 1e2982_qp, 1e2983_qp, 1e2984_qp, 1e2985_qp, &
        1e2986_qp, 1e2987_qp, 1e2988_qp, 1e2989_qp, 1e2990_qp, 1e2991_qp, &
        1e2992_qp, 1e2993_qp, 1e2994_qp, 1e2995_qp, 1e2996_qp, 1e2997_qp, &
        1e2998_qp, 1e2999_qp, 1e3000_qp, 1e3001_qp, 1e3002_qp, 1e3003_qp, &
        1e3004_qp, 1e3005_qp, 1e3006_qp, 1e3007_qp, 1e3008_qp, 1e3009_qp, &
        1e3010_qp, 1e3011_qp, 1e3012_qp, 1e3013_qp, 1e3014_qp, 1e3015_qp, &
        1e3016_qp, 1e3017_qp, 1e3018_qp, 1e3019_qp, 1e3020_qp, 1e3021_qp, &
        1e3022_qp, 1e3023_qp, 1e3024_qp, 1e3025_qp, 1e3026_qp, 1e3027_qp, &
        1e3028_qp, 1e3029_qp, 1e3030_qp, 1e3031_qp, 1e3032_qp, 1e3033_qp, &
        1e3034_qp, 1e3035_qp, 1e3036_qp, 1e3037_qp, 1e3038_qp, 1e3039_qp, &
        1e3040_qp, 1e3041_qp, 1e3042_qp, 1e3043_qp, 1e3044_qp, 1e3045_qp, &
        1e3046_qp, 1e3047_qp, 1e3048_qp, 1e3049_qp, 1e3050_qp, 1e3051_qp, &
        1e3052_qp, 1e3053_qp, 1e3054_qp, 1e3055_qp, 1e3056_qp, 1e3057_qp, &
        1e3058_qp, 1e3059_qp, 1e3060_qp, 1e3061_qp, 1e3062_qp, 1e3063_qp, &
        1e3064_qp, 1e3065_qp, 1e3066_qp, 1e3067_qp, 1e3068_qp, 1e3069_qp, &
        1e3070_qp, 1e3071_qp, 1e3072_qp, 1e3073_qp, 1e3074_qp, 1e3075_qp, &
        1e3076_qp, 1e3077_qp, 1e3078_qp, 1e3079_qp, 1e3080_qp, 1e3081_qp, &
        1e3082_qp, 1e3083_qp, 1e3084_qp, 1e3085_qp, 1e3086_qp, 1e3087_qp, &
        1e3088_qp, 1e3089_qp, 1e3090_qp, 1e3091_qp, 1e3092_qp, 1e3093_qp, &
        1e3094_qp, 1e3095_qp, 1e3096_qp, 1e3097_qp, 1e3098_qp, 1e3099_qp, &
        1e3100_qp, 1e3101_qp, 1e3102_qp, 1e3103_qp, 1e3104_qp, 1e3105_qp, &
        1e3106_qp, 1e3107_qp, 1e3108_qp, 1e3109_qp, 1e3110_qp, 1e3111_qp, &
        1e3112_qp, 1e3113_qp, 1e3114_qp, 1e3115_qp, 1e3116_qp, 1e3117_qp, &
        1e3118_qp, 1e3119_qp, 1e3120_qp, 1e3121_qp, 1e3122_qp, 1e3123_qp, &
        1e3124_qp, 1e3125_qp, 1e3126_qp, 1e3127_qp, 1e3128_qp, 1e3129_qp, &
        1e3130_qp, 1e3131_qp, 1e3132_qp, 1e3133_qp, 1e3134_qp, 1e3135_qp, &
        1e3136_qp, 1e3137_qp, 1e3138_qp, 1e3139_qp, 1e3140_qp, 1e3141_qp, &
        1e3142_qp, 1e3143_qp, 1e3144_qp, 1e3145_qp, 1e3146_qp, 1e3147_qp, &
        1e3148_qp, 1e3149_qp, 1e3150_qp, 1e3151_qp, 1e3152_qp, 1e3153_qp, &
        1e3154_qp, 1e3155_qp, 1e3156_qp, 1e3157_qp, 1e3158_qp, 1e3159_qp, &
        1e3160_qp, 1e3161_qp, 1e3162_qp, 1e3163_qp, 1e3164_qp, 1e3165_qp, &
        1e3166_qp, 1e3167_qp, 1e3168_qp, 1e3169_qp, 1e3170_qp, 1e3171_qp, &
        1e3172_qp, 1e3173_qp, 1e3174_qp, 1e3175_qp, 1e3176_qp, 1e3177_qp, &
        1e3178_qp, 1e3179_qp, 1e3180_qp, 1e3181_qp, 1e3182_qp, 1e3183_qp, &
        1e3184_qp, 1e3185_qp, 1e3186_qp, 1e3187_qp, 1e3188_qp, 1e3189_qp, &
        1e3190_qp, 1e3191_qp, 1e3192_qp, 1e3193_qp, 1e3194_qp, 1e3195_qp, &
        1e3196_qp, 1e3197_qp, 1e3198_qp, 1e3199_qp, 1e3200_qp, 1e3201_qp, &
        1e3202_qp, 1e3203_qp, 1e3204_qp, 1e3205_qp, 1e3206_qp, 1e3207_qp, &
        1e3208_qp, 1e3209_qp, 1e3210_qp, 1e3211_qp, 1e3212_qp, 1e3213_qp, &
        1e3214_qp, 1e3215_qp, 1e3216_qp, 1e3217_qp, 1e3218_qp, 1e3219_qp, &
        1e3220_qp, 1e3221_qp, 1e3222_qp, 1e3223_qp, 1e3224_qp, 1e3225_qp, &
        1e3226_qp, 1e3227_qp, 1e3228_qp, 1e3229_qp, 1e3230_qp, 1e3231_qp, &
        1e3232_qp, 1e3233_qp, 1e3234_qp, 1e3235_qp, 1e3236_qp, 1e3237_qp, &
        1e3238_qp, 1e3239_qp, 1e3240_qp, 1e3241_qp, 1e3242_qp, 1e3243_qp, &
        1e3244_qp, 1e3245_qp, 1e3246_qp, 1e3247_qp, 1e3248_qp, 1e3249_qp, &
        1e3250_qp, 1e3251_qp, 1e3252_qp, 1e3253_qp, 1e3254_qp, 1e3255_qp, &
        1e3256_qp, 1e3257_qp, 1e3258_qp, 1e3259_qp, 1e3260_qp, 1e3261_qp, &
        1e3262_qp, 1e3263_qp, 1e3264_qp, 1e3265_qp, 1e3266_qp, 1e3267_qp, &
        1e3268_qp, 1e3269_qp, 1e3270_qp, 1e3271_qp, 1e3272_qp, 1e3273_qp, &
        1e3274_qp, 1e3275_qp, 1e3276_qp, 1e3277_qp, 1e3278_qp, 1e3279_qp, &
        1e3280_qp, 1e3281_qp, 1e3282_qp, 1e3283_qp, 1e3284_qp, 1e3285_qp, &
        1e3286_qp, 1e3287_qp, 1e3288_qp, 1e3289_qp, 1e3290_qp, 1e3291_qp, &
        1e3292_qp, 1e3293_qp, 1e3294_qp, 1e3295_qp, 1e3296_qp, 1e3297_qp, &
        1e3298_qp, 1e3299_qp, 1e3300_qp, 1e3301_qp, 1e3302_qp, 1e3303_qp, &
        1e3304_qp, 1e3305_qp, 1e3306_qp, 1e3307_qp, 1e3308_qp, 1e3309_qp, &
        1e3310_qp, 1e3311_qp, 1e3312_qp, 1e3313_qp, 1e3314_qp, 1e3315_qp, &
        1e3316_qp, 1e3317_qp, 1e3318_qp, 1e3319_qp, 1e3320_qp, 1e3321_qp, &
        1e3322_qp, 1e3323_qp, 1e3324_qp, 1e3325_qp, 1e3326_qp, 1e3327_qp, &
        1e3328_qp, 1e3329_qp, 1e3330_qp, 1e3331_qp, 1e3332_qp, 1e3333_qp, &
        1e3334_qp, 1e3335_qp, 1e3336_qp, 1e3337_qp, 1e3338_qp, 1e3339_qp, &
        1e3340_qp, 1e3341_qp, 1e3342_qp, 1e3343_qp, 1e3344_qp, 1e3345_qp, &
        1e3346_qp, 1e3347_qp, 1e3348_qp, 1e3349_qp, 1e3350_qp, 1e3351_qp, &
        1e3352_qp, 1e3353_qp, 1e3354_qp, 1e3355_qp, 1e3356_qp, 1e3357_qp, &
        1e3358_qp, 1e3359_qp, 1e3360_qp, 1e3361_qp, 1e3362_qp, 1e3363_qp, &
        1e3364_qp, 1e3365_qp, 1e3366_qp, 1e3367_qp, 1e3368_qp, 1e3369_qp, &
        1e3370_qp, 1e3371_qp, 1e3372_qp, 1e3373_qp, 1e3374_qp, 1e3375_qp, &
        1e3376_qp, 1e3377_qp, 1e3378_qp, 1e3379_qp, 1e3380_qp, 1e3381_qp, &
        1e3382_qp, 1e3383_qp, 1e3384_qp, 1e3385_qp, 1e3386_qp, 1e3387_qp, &
        1e3388_qp, 1e3389_qp, 1e3390_qp, 1e3391_qp, 1e3392_qp, 1e3393_qp, &
        1e3394_qp, 1e3395_qp, 1e3396_qp, 1e3397_qp, 1e3398_qp, 1e3399_qp, &
        1e3400_qp, 1e3401_qp, 1e3402_qp, 1e3403_qp, 1e3404_qp, 1e3405_qp, &
        1e3406_qp, 1e3407_qp, 1e3408_qp, 1e3409_qp, 1e3410_qp, 1e3411_qp, &
        1e3412_qp, 1e3413_qp, 1e3414_qp, 1e3415_qp, 1e3416_qp, 1e3417_qp, &
        1e3418_qp, 1e3419_qp, 1e3420_qp, 1e3421_qp, 1e3422_qp, 1e3423_qp, &
        1e3424_qp, 1e3425_qp, 1e3426_qp, 1e3427_qp, 1e3428_qp, 1e3429_qp, &
        1e3430_qp, 1e3431_qp, 1e3432_qp, 1e3433_qp, 1e3434_qp, 1e3435_qp, &
        1e3436_qp, 1e3437_qp, 1e3438_qp, 1e3439_qp, 1e3440_qp, 1e3441_qp, &
        1e3442_qp, 1e3443_qp, 1e3444_qp, 1e3445_qp, 1e3446_qp, 1e3447_qp, &
        1e3448_qp, 1e3449_qp, 1e3450_qp, 1e3451_qp, 1e3452_qp, 1e3453_qp, &
        1e3454_qp, 1e3455_qp, 1e3456_qp, 1e3457_qp, 1e3458_qp, 1e3459_qp, &
        1e3460_qp, 1e3461_qp, 1e3462_qp, 1e3463_qp, 1e3464_qp, 1e3465_qp, &
        1e3466_qp, 1e3467_qp, 1e3468_qp, 1e3469_qp, 1e3470_qp, 1e3471_qp, &
        1e3472_qp, 1e3473_qp, 1e3474_qp, 1e3475_qp, 1e3476_qp, 1e3477_qp, &
        1e3478_qp, 1e3479_qp, 1e3480_qp, 1e3481_qp, 1e3482_qp, 1e3483_qp, &
        1e3484_qp, 1e3485_qp, 1e3486_qp, 1e3487_qp, 1e3488_qp, 1e3489_qp, &
        1e3490_qp, 1e3491_qp, 1e3492_qp, 1e3493_qp, 1e3494_qp, 1e3495_qp, &
        1e3496_qp, 1e3497_qp, 1e3498_qp, 1e3499_qp, 1e3500_qp, 1e3501_qp, &
        1e3502_qp, 1e3503_qp, 1e3504_qp, 1e3505_qp, 1e3506_qp, 1e3507_qp, &
        1e3508_qp, 1e3509_qp, 1e3510_qp, 1e3511_qp, 1e3512_qp, 1e3513_qp, &
        1e3514_qp, 1e3515_qp, 1e3516_qp, 1e3517_qp, 1e3518_qp, 1e3519_qp, &
        1e3520_qp, 1e3521_qp, 1e3522_qp, 1e3523_qp, 1e3524_qp, 1e3525_qp, &
        1e3526_qp, 1e3527_qp, 1e3528_qp, 1e3529_qp, 1e3530_qp, 1e3531_qp, &
        1e3532_qp, 1e3533_qp, 1e3534_qp, 1e3535_qp, 1e3536_qp, 1e3537_qp &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS7(*) = [ &
        1e3538_qp, 1e3539_qp, 1e3540_qp, 1e3541_qp, 1e3542_qp, 1e3543_qp, &
        1e3544_qp, 1e3545_qp, 1e3546_qp, 1e3547_qp, 1e3548_qp, 1e3549_qp, &
        1e3550_qp, 1e3551_qp, 1e3552_qp, 1e3553_qp, 1e3554_qp, 1e3555_qp, &
        1e3556_qp, 1e3557_qp, 1e3558_qp, 1e3559_qp, 1e3560_qp, 1e3561_qp, &
        1e3562_qp, 1e3563_qp, 1e3564_qp, 1e3565_qp, 1e3566_qp, 1e3567_qp, &
        1e3568_qp, 1e3569_qp, 1e3570_qp, 1e3571_qp, 1e3572_qp, 1e3573_qp, &
        1e3574_qp, 1e3575_qp, 1e3576_qp, 1e3577_qp, 1e3578_qp, 1e3579_qp, &
        1e3580_qp, 1e3581_qp, 1e3582_qp, 1e3583_qp, 1e3584_qp, 1e3585_qp, &
        1e3586_qp, 1e3587_qp, 1e3588_qp, 1e3589_qp, 1e3590_qp, 1e3591_qp, &
        1e3592_qp, 1e3593_qp, 1e3594_qp, 1e3595_qp, 1e3596_qp, 1e3597_qp, &
        1e3598_qp, 1e3599_qp, 1e3600_qp, 1e3601_qp, 1e3602_qp, 1e3603_qp, &
        1e3604_qp, 1e3605_qp, 1e3606_qp, 1e3607_qp, 1e3608_qp, 1e3609_qp, &
        1e3610_qp, 1e3611_qp, 1e3612_qp, 1e3613_qp, 1e3614_qp, 1e3615_qp, &
        1e3616_qp, 1e3617_qp, 1e3618_qp, 1e3619_qp, 1e3620_qp, 1e3621_qp, &
        1e3622_qp, 1e3623_qp, 1e3624_qp, 1e3625_qp, 1e3626_qp, 1e3627_qp, &
        1e3628_qp, 1e3629_qp, 1e3630_qp, 1e3631_qp, 1e3632_qp, 1e3633_qp, &
        1e3634_qp, 1e3635_qp, 1e3636_qp, 1e3637_qp, 1e3638_qp, 1e3639_qp, &
        1e3640_qp, 1e3641_qp, 1e3642_qp, 1e3643_qp, 1e3644_qp, 1e3645_qp, &
        1e3646_qp, 1e3647_qp, 1e3648_qp, 1e3649_qp, 1e3650_qp, 1e3651_qp, &
        1e3652_qp, 1e3653_qp, 1e3654_qp, 1e3655_qp, 1e3656_qp, 1e3657_qp, &
        1e3658_qp, 1e3659_qp, 1e3660_qp, 1e3661_qp, 1e3662_qp, 1e3663_qp, &
        1e3664_qp, 1e3665_qp, 1e3666_qp, 1e3667_qp, 1e3668_qp, 1e3669_qp, &
        1e3670_qp, 1e3671_qp, 1e3672_qp, 1e3673_qp, 1e3674_qp, 1e3675_qp, &
        1e3676_qp, 1e3677_qp, 1e3678_qp, 1e3679_qp, 1e3680_qp, 1e3681_qp, &
        1e3682_qp, 1e3683_qp, 1e3684_qp, 1e3685_qp, 1e3686_qp, 1e3687_qp, &
        1e3688_qp, 1e3689_qp, 1e3690_qp, 1e3691_qp, 1e3692_qp, 1e3693_qp, &
        1e3694_qp, 1e3695_qp, 1e3696_qp, 1e3697_qp, 1e3698_qp, 1e3699_qp, &
        1e3700_qp, 1e3701_qp, 1e3702_qp, 1e3703_qp, 1e3704_qp, 1e3705_qp, &
        1e3706_qp, 1e3707_qp, 1e3708_qp, 1e3709_qp, 1e3710_qp, 1e3711_qp, &
        1e3712_qp, 1e3713_qp, 1e3714_qp, 1e3715_qp, 1e3716_qp, 1e3717_qp, &
        1e3718_qp, 1e3719_qp, 1e3720_qp, 1e3721_qp, 1e3722_qp, 1e3723_qp, &
        1e3724_qp, 1e3725_qp, 1e3726_qp, 1e3727_qp, 1e3728_qp, 1e3729_qp, &
        1e3730_qp, 1e3731_qp, 1e3732_qp, 1e3733_qp, 1e3734_qp, 1e3735_qp, &
        1e3736_qp, 1e3737_qp, 1e3738_qp, 1e3739_qp, 1e3740_qp, 1e3741_qp, &
        1e3742_qp, 1e3743_qp, 1e3744_qp, 1e3745_qp, 1e3746_qp, 1e3747_qp, &
        1e3748_qp, 1e3749_qp, 1e3750_qp, 1e3751_qp, 1e3752_qp, 1e3753_qp, &
        1e3754_qp, 1e3755_qp, 1e3756_qp, 1e3757_qp, 1e3758_qp, 1e3759_qp, &
        1e3760_qp, 1e3761_qp, 1e3762_qp, 1e3763_qp, 1e3764_qp, 1e3765_qp, &
        1e3766_qp, 1e3767_qp, 1e3768_qp, 1e3769_qp, 1e3770_qp, 1e3771_qp, &
        1e3772_qp, 1e3773_qp, 1e3774_qp, 1e3775_qp, 1e3776_qp, 1e3777_qp, &
        1e3778_qp, 1e3779_qp, 1e3780_qp, 1e3781_qp, 1e3782_qp, 1e3783_qp, &
        1e3784_qp, 1e3785_qp, 1e3786_qp, 1e3787_qp, 1e3788_qp, 1e3789_qp, &
        1e3790_qp, 1e3791_qp, 1e3792_qp, 1e3793_qp, 1e3794_qp, 1e3795_qp, &
        1e3796_qp, 1e3797_qp, 1e3798_qp, 1e3799_qp, 1e3800_qp, 1e3801_qp, &
        1e3802_qp, 1e3803_qp, 1e3804_qp, 1e3805_qp, 1e3806_qp, 1e3807_qp, &
        1e3808_qp, 1e3809_qp, 1e3810_qp, 1e3811_qp, 1e3812_qp, 1e3813_qp, &
        1e3814_qp, 1e3815_qp, 1e3816_qp, 1e3817_qp, 1e3818_qp, 1e3819_qp, &
        1e3820_qp, 1e3821_qp, 1e3822_qp, 1e3823_qp, 1e3824_qp, 1e3825_qp, &
        1e3826_qp, 1e3827_qp, 1e3828_qp, 1e3829_qp, 1e3830_qp, 1e3831_qp, &
        1e3832_qp, 1e3833_qp, 1e3834_qp, 1e3835_qp, 1e3836_qp, 1e3837_qp, &
        1e3838_qp, 1e3839_qp, 1e3840_qp, 1e3841_qp, 1e3842_qp, 1e3843_qp, &
        1e3844_qp, 1e3845_qp, 1e3846_qp, 1e3847_qp, 1e3848_qp, 1e3849_qp, &
        1e3850_qp, 1e3851_qp, 1e3852_qp, 1e3853_qp, 1e3854_qp, 1e3855_qp, &
        1e3856_qp, 1e3857_qp, 1e3858_qp, 1e3859_qp, 1e3860_qp, 1e3861_qp, &
        1e3862_qp, 1e3863_qp, 1e3864_qp, 1e3865_qp, 1e3866_qp, 1e3867_qp, &
        1e3868_qp, 1e3869_qp, 1e3870_qp, 1e3871_qp, 1e3872_qp, 1e3873_qp, &
        1e3874_qp, 1e3875_qp, 1e3876_qp, 1e3877_qp, 1e3878_qp, 1e3879_qp, &
        1e3880_qp, 1e3881_qp, 1e3882_qp, 1e3883_qp, 1e3884_qp, 1e3885_qp, &
        1e3886_qp, 1e3887_qp, 1e3888_qp, 1e3889_qp, 1e3890_qp, 1e3891_qp, &
        1e3892_qp, 1e3893_qp, 1e3894_qp, 1e3895_qp, 1e3896_qp, 1e3897_qp, &
        1e3898_qp, 1e3899_qp, 1e3900_qp, 1e3901_qp, 1e3902_qp, 1e3903_qp, &
        1e3904_qp, 1e3905_qp, 1e3906_qp, 1e3907_qp, 1e3908_qp, 1e3909_qp, &
        1e3910_qp, 1e3911_qp, 1e3912_qp, 1e3913_qp, 1e3914_qp, 1e3915_qp, &
        1e3916_qp, 1e3917_qp, 1e3918_qp, 1e3919_qp, 1e3920_qp, 1e3921_qp, &
        1e3922_qp, 1e3923_qp, 1e3924_qp, 1e3925_qp, 1e3926_qp, 1e3927_qp, &
        1e3928_qp, 1e3929_qp, 1e3930_qp, 1e3931_qp, 1e3932_qp, 1e3933_qp, &
        1e3934_qp, 1e3935_qp, 1e3936_qp, 1e3937_qp, 1e3938_qp, 1e3939_qp, &
        1e3940_qp, 1e3941_qp, 1e3942_qp, 1e3943_qp, 1e3944_qp, 1e3945_qp, &
        1e3946_qp, 1e3947_qp, 1e3948_qp, 1e3949_qp, 1e3950_qp, 1e3951_qp, &
        1e3952_qp, 1e3953_qp, 1e3954_qp, 1e3955_qp, 1e3956_qp, 1e3957_qp, &
        1e3958_qp, 1e3959_qp, 1e3960_qp, 1e3961_qp, 1e3962_qp, 1e3963_qp, &
        1e3964_qp, 1e3965_qp, 1e3966_qp, 1e3967_qp, 1e3968_qp, 1e3969_qp, &
        1e3970_qp, 1e3971_qp, 1e3972_qp, 1e3973_qp, 1e3974_qp, 1e3975_qp, &
        1e3976_qp, 1e3977_qp, 1e3978_qp, 1e3979_qp, 1e3980_qp, 1e3981_qp, &
        1e3982_qp, 1e3983_qp, 1e3984_qp, 1e3985_qp, 1e3986_qp, 1e3987_qp, &
        1e3988_qp, 1e3989_qp, 1e3990_qp, 1e3991_qp, 1e3992_qp, 1e3993_qp, &
        1e3994_qp, 1e3995_qp, 1e3996_qp, 1e3997_qp, 1e3998_qp, 1e3999_qp, &
        1e4000_qp, 1e4001_qp, 1e4002_qp, 1e4003_qp, 1e4004_qp, 1e4005_qp, &
        1e4006_qp, 1e4007_qp, 1e4008_qp, 1e4009_qp, 1e4010_qp, 1e4011_qp, &
        1e4012_qp, 1e4013_qp, 1e4014_qp, 1e4015_qp, 1e4016_qp, 1e4017_qp, &
        1e4018_qp, 1e4019_qp, 1e4020_qp, 1e4021_qp, 1e4022_qp, 1e4023_qp, &
        1e4024_qp, 1e4025_qp, 1e4026_qp, 1e4027_qp, 1e4028_qp, 1e4029_qp, &
        1e4030_qp, 1e4031_qp, 1e4032_qp, 1e4033_qp, 1e4034_qp, 1e4035_qp, &
        1e4036_qp, 1e4037_qp, 1e4038_qp, 1e4039_qp, 1e4040_qp, 1e4041_qp, &
        1e4042_qp, 1e4043_qp, 1e4044_qp, 1e4045_qp, 1e4046_qp, 1e4047_qp, &
        1e4048_qp, 1e4049_qp, 1e4050_qp, 1e4051_qp, 1e4052_qp, 1e4053_qp, &
        1e4054_qp, 1e4055_qp, 1e4056_qp, 1e4057_qp, 1e4058_qp, 1e4059_qp, &
        1e4060_qp, 1e4061_qp, 1e4062_qp, 1e4063_qp, 1e4064_qp, 1e4065_qp, &
        1e4066_qp, 1e4067_qp, 1e4068_qp, 1e4069_qp, 1e4070_qp, 1e4071_qp, &
        1e4072_qp, 1e4073_qp, 1e4074_qp, 1e4075_qp, 1e4076_qp, 1e4077_qp, &
        1e4078_qp, 1e4079_qp, 1e4080_qp, 1e4081_qp, 1e4082_qp, 1e4083_qp, &
        1e4084_qp, 1e4085_qp, 1e4086_qp, 1e4087_qp, 1e4088_qp, 1e4089_qp, &
        1e4090_qp, 1e4091_qp, 1e4092_qp, 1e4093_qp, 1e4094_qp, 1e4095_qp, &
        1e4096_qp, 1e4097_qp, 1e4098_qp, 1e4099_qp, 1e4100_qp, 1e4101_qp, &
        1e4102_qp, 1e4103_qp, 1e4104_qp, 1e4105_qp, 1e4106_qp, 1e4107_qp, &
        1e4108_qp, 1e4109_qp, 1e4110_qp, 1e4111_qp, 1e4112_qp, 1e4113_qp, &
        1e4114_qp, 1e4115_qp, 1e4116_qp, 1e4117_qp, 1e4118_qp, 1e4119_qp, &
        1e4120_qp, 1e4121_qp, 1e4122_qp, 1e4123_qp, 1e4124_qp, 1e4125_qp, &
        1e4126_qp, 1e4127_qp, 1e4128_qp, 1e4129_qp, 1e4130_qp, 1e4131_qp, &
        1e4132_qp, 1e4133_qp, 1e4134_qp, 1e4135_qp, 1e4136_qp, 1e4137_qp, &
        1e4138_qp, 1e4139_qp, 1e4140_qp, 1e4141_qp, 1e4142_qp, 1e4143_qp, &
        1e4144_qp, 1e4145_qp, 1e4146_qp, 1e4147_qp, 1e4148_qp, 1e4149_qp, &
        1e4150_qp, 1e4151_qp, 1e4152_qp, 1e4153_qp, 1e4154_qp, 1e4155_qp, &
        1e4156_qp, 1e4157_qp, 1e4158_qp, 1e4159_qp, 1e4160_qp, 1e4161_qp, &
        1e4162_qp, 1e4163_qp, 1e4164_qp, 1e4165_qp, 1e4166_qp, 1e4167_qp, &
        1e4168_qp, 1e4169_qp, 1e4170_qp, 1e4171_qp, 1e4172_qp, 1e4173_qp, &
        1e4174_qp, 1e4175_qp, 1e4176_qp, 1e4177_qp, 1e4178_qp, 1e4179_qp, &
        1e4180_qp, 1e4181_qp, 1e4182_qp, 1e4183_qp, 1e4184_qp, 1e4185_qp, &
        1e4186_qp, 1e4187_qp, 1e4188_qp, 1e4189_qp, 1e4190_qp, 1e4191_qp, &
        1e4192_qp, 1e4193_qp, 1e4194_qp, 1e4195_qp, 1e4196_qp, 1e4197_qp, &
        1e4198_qp, 1e4199_qp, 1e4200_qp, 1e4201_qp, 1e4202_qp, 1e4203_qp, &
        1e4204_qp, 1e4205_qp, 1e4206_qp, 1e4207_qp, 1e4208_qp, 1e4209_qp, &
        1e4210_qp, 1e4211_qp, 1e4212_qp, 1e4213_qp, 1e4214_qp, 1e4215_qp, &
        1e4216_qp, 1e4217_qp, 1e4218_qp, 1e4219_qp, 1e4220_qp, 1e4221_qp, &
        1e4222_qp, 1e4223_qp, 1e4224_qp, 1e4225_qp, 1e4226_qp, 1e4227_qp, &
        1e4228_qp, 1e4229_qp, 1e4230_qp, 1e4231_qp, 1e4232_qp, 1e4233_qp, &
        1e4234_qp, 1e4235_qp, 1e4236_qp, 1e4237_qp, 1e4238_qp, 1e4239_qp, &
        1e4240_qp, 1e4241_qp, 1e4242_qp, 1e4243_qp, 1e4244_qp, 1e4245_qp, &
        1e4246_qp, 1e4247_qp, 1e4248_qp, 1e4249_qp, 1e4250_qp, 1e4251_qp, &
        1e4252_qp, 1e4253_qp, 1e4254_qp, 1e4255_qp, 1e4256_qp, 1e4257_qp, &
        1e4258_qp, 1e4259_qp, 1e4260_qp, 1e4261_qp, 1e4262_qp, 1e4263_qp, &
        1e4264_qp, 1e4265_qp, 1e4266_qp, 1e4267_qp, 1e4268_qp, 1e4269_qp, &
        1e4270_qp, 1e4271_qp, 1e4272_qp, 1e4273_qp, 1e4274_qp, 1e4275_qp, &
        1e4276_qp, 1e4277_qp, 1e4278_qp, 1e4279_qp, 1e4280_qp, 1e4281_qp, &
        1e4282_qp, 1e4283_qp, 1e4284_qp, 1e4285_qp, 1e4286_qp, 1e4287_qp, &
        1e4288_qp, 1e4289_qp, 1e4290_qp, 1e4291_qp, 1e4292_qp, 1e4293_qp, &
        1e4294_qp, 1e4295_qp, 1e4296_qp, 1e4297_qp, 1e4298_qp, 1e4299_qp, &
        1e4300_qp, 1e4301_qp, 1e4302_qp, 1e4303_qp, 1e4304_qp, 1e4305_qp, &
        1e4306_qp, 1e4307_qp, 1e4308_qp, 1e4309_qp, 1e4310_qp, 1e4311_qp, &
        1e4312_qp, 1e4313_qp, 1e4314_qp, 1e4315_qp, 1e4316_qp, 1e4317_qp, &
        1e4318_qp, 1e4319_qp, 1e4320_qp, 1e4321_qp, 1e4322_qp, 1e4323_qp, &
        1e4324_qp, 1e4325_qp, 1e4326_qp, 1e4327_qp, 1e4328_qp, 1e4329_qp, &
        1e4330_qp, 1e4331_qp, 1e4332_qp, 1e4333_qp, 1e4334_qp, 1e4335_qp, &
        1e4336_qp, 1e4337_qp, 1e4338_qp, 1e4339_qp, 1e4340_qp, 1e4341_qp, &
        1e4342_qp, 1e4343_qp, 1e4344_qp, 1e4345_qp, 1e4346_qp, 1e4347_qp, &
        1e4348_qp, 1e4349_qp, 1e4350_qp, 1e4351_qp, 1e4352_qp, 1e4353_qp, &
        1e4354_qp, 1e4355_qp, 1e4356_qp, 1e4357_qp, 1e4358_qp, 1e4359_qp, &
        1e4360_qp, 1e4361_qp, 1e4362_qp, 1e4363_qp, 1e4364_qp, 1e4365_qp, &
        1e4366_qp, 1e4367_qp, 1e4368_qp, 1e4369_qp, 1e4370_qp, 1e4371_qp, &
        1e4372_qp, 1e4373_qp, 1e4374_qp, 1e4375_qp, 1e4376_qp, 1e4377_qp, &
        1e4378_qp, 1e4379_qp, 1e4380_qp, 1e4381_qp, 1e4382_qp, 1e4383_qp, &
        1e4384_qp, 1e4385_qp, 1e4386_qp, 1e4387_qp, 1e4388_qp, 1e4389_qp, &
        1e4390_qp, 1e4391_qp, 1e4392_qp, 1e4393_qp, 1e4394_qp, 1e4395_qp, &
        1e4396_qp, 1e4397_qp, 1e4398_qp, 1e4399_qp, 1e4400_qp, 1e4401_qp, &
        1e4402_qp, 1e4403_qp, 1e4404_qp, 1e4405_qp, 1e4406_qp, 1e4407_qp, &
        1e4408_qp, 1e4409_qp, 1e4410_qp, 1e4411_qp, 1e4412_qp, 1e4413_qp, &
        1e4414_qp, 1e4415_qp, 1e4416_qp, 1e4417_qp, 1e4418_qp, 1e4419_qp, &
        1e4420_qp, 1e4421_qp, 1e4422_qp, 1e4423_qp, 1e4424_qp, 1e4425_qp, &
        1e4426_qp, 1e4427_qp, 1e4428_qp, 1e4429_qp, 1e4430_qp, 1e4431_qp, &
        1e4432_qp, 1e4433_qp, 1e4434_qp, 1e4435_qp, 1e4436_qp, 1e4437_qp, &
        1e4438_qp, 1e4439_qp, 1e4440_qp, 1e4441_qp, 1e4442_qp, 1e4443_qp, &
        1e4444_qp, 1e4445_qp, 1e4446_qp, 1e4447_qp, 1e4448_qp, 1e4449_qp, &
        1e4450_qp, 1e4451_qp, 1e4452_qp, 1e4453_qp, 1e4454_qp, 1e4455_qp, &
        1e4456_qp, 1e4457_qp, 1e4458_qp, 1e4459_qp, 1e4460_qp, 1e4461_qp, &
        1e4462_qp, 1e4463_qp, 1e4464_qp, 1e4465_qp, 1e4466_qp, 1e4467_qp, &
        1e4468_qp, 1e4469_qp, 1e4470_qp, 1e4471_qp, 1e4472_qp, 1e4473_qp, &
        1e4474_qp, 1e4475_qp, 1e4476_qp, 1e4477_qp, 1e4478_qp, 1e4479_qp, &
        1e4480_qp, 1e4481_qp, 1e4482_qp, 1e4483_qp, 1e4484_qp, 1e4485_qp, &
        1e4486_qp, 1e4487_qp, 1e4488_qp, 1e4489_qp, 1e4490_qp, 1e4491_qp, &
        1e4492_qp, 1e4493_qp, 1e4494_qp, 1e4495_qp, 1e4496_qp, 1e4497_qp, &
        1e4498_qp, 1e4499_qp, 1e4500_qp, 1e4501_qp, 1e4502_qp, 1e4503_qp, &
        1e4504_qp, 1e4505_qp, 1e4506_qp, 1e4507_qp, 1e4508_qp, 1e4509_qp, &
        1e4510_qp, 1e4511_qp, 1e4512_qp, 1e4513_qp, 1e4514_qp, 1e4515_qp, &
        1e4516_qp, 1e4517_qp, 1e4518_qp, 1e4519_qp, 1e4520_qp, 1e4521_qp, &
        1e4522_qp, 1e4523_qp, 1e4524_qp, 1e4525_qp, 1e4526_qp, 1e4527_qp, &
        1e4528_qp, 1e4529_qp, 1e4530_qp, 1e4531_qp, 1e4532_qp, 1e4533_qp, &
        1e4534_qp, 1e4535_qp, 1e4536_qp, 1e4537_qp, 1e4538_qp, 1e4539_qp, &
        1e4540_qp, 1e4541_qp, 1e4542_qp, 1e4543_qp, 1e4544_qp, 1e4545_qp, &
        1e4546_qp, 1e4547_qp, 1e4548_qp, 1e4549_qp, 1e4550_qp, 1e4551_qp, &
        1e4552_qp, 1e4553_qp, 1e4554_qp, 1e4555_qp, 1e4556_qp, 1e4557_qp, &
        1e4558_qp, 1e4559_qp, 1e4560_qp, 1e4561_qp, 1e4562_qp, 1e4563_qp, &
        1e4564_qp, 1e4565_qp, 1e4566_qp, 1e4567_qp, 1e4568_qp, 1e4569_qp, &
        1e4570_qp, 1e4571_qp, 1e4572_qp, 1e4573_qp, 1e4574_qp, 1e4575_qp, &
        1e4576_qp, 1e4577_qp, 1e4578_qp, 1e4579_qp, 1e4580_qp, 1e4581_qp, &
        1e4582_qp, 1e4583_qp, 1e4584_qp, 1e4585_qp, 1e4586_qp, 1e4587_qp, &
        1e4588_qp, 1e4589_qp, 1e4590_qp, 1e4591_qp, 1e4592_qp, 1e4593_qp, &
        1e4594_qp, 1e4595_qp, 1e4596_qp, 1e4597_qp, 1e4598_qp, 1e4599_qp, &
        1e4600_qp, 1e4601_qp, 1e4602_qp, 1e4603_qp, 1e4604_qp, 1e4605_qp, &
        1e4606_qp, 1e4607_qp, 1e4608_qp, 1e4609_qp, 1e4610_qp, 1e4611_qp, &
        1e4612_qp, 1e4613_qp, 1e4614_qp, 1e4615_qp, 1e4616_qp, 1e4617_qp, &
        1e4618_qp, 1e4619_qp, 1e4620_qp, 1e4621_qp, 1e4622_qp, 1e4623_qp, &
        1e4624_qp, 1e4625_qp, 1e4626_qp, 1e4627_qp, 1e4628_qp, 1e4629_qp, &
        1e4630_qp, 1e4631_qp, 1e4632_qp, 1e4633_qp, 1e4634_qp, 1e4635_qp, &
        1e4636_qp, 1e4637_qp, 1e4638_qp, 1e4639_qp, 1e4640_qp, 1e4641_qp, &
        1e4642_qp, 1e4643_qp, 1e4644_qp, 1e4645_qp, 1e4646_qp, 1e4647_qp, &
        1e4648_qp, 1e4649_qp, 1e4650_qp, 1e4651_qp, 1e4652_qp, 1e4653_qp, &
        1e4654_qp, 1e4655_qp, 1e4656_qp, 1e4657_qp, 1e4658_qp, 1e4659_qp, &
        1e4660_qp, 1e4661_qp, 1e4662_qp, 1e4663_qp, 1e4664_qp, 1e4665_qp, &
        1e4666_qp, 1e4667_qp, 1e4668_qp, 1e4669_qp, 1e4670_qp, 1e4671_qp, &
        1e4672_qp, 1e4673_qp, 1e4674_qp, 1e4675_qp, 1e4676_qp, 1e4677_qp, &
        1e4678_qp, 1e4679_qp, 1e4680_qp, 1e4681_qp, 1e4682_qp, 1e4683_qp, &
        1e4684_qp, 1e4685_qp, 1e4686_qp, 1e4687_qp, 1e4688_qp, 1e4689_qp, &
        1e4690_qp, 1e4691_qp, 1e4692_qp, 1e4693_qp, 1e4694_qp, 1e4695_qp, &
        1e4696_qp, 1e4697_qp, 1e4698_qp, 1e4699_qp, 1e4700_qp, 1e4701_qp, &
        1e4702_qp, 1e4703_qp, 1e4704_qp, 1e4705_qp, 1e4706_qp, 1e4707_qp, &
        1e4708_qp, 1e4709_qp, 1e4710_qp, 1e4711_qp, 1e4712_qp, 1e4713_qp, &
        1e4714_qp, 1e4715_qp, 1e4716_qp, 1e4717_qp, 1e4718_qp, 1e4719_qp, &
        1e4720_qp, 1e4721_qp, 1e4722_qp, 1e4723_qp, 1e4724_qp, 1e4725_qp, &
        1e4726_qp, 1e4727_qp, 1e4728_qp, 1e4729_qp, 1e4730_qp, 1e4731_qp, &
        1e4732_qp, 1e4733_qp, 1e4734_qp, 1e4735_qp, 1e4736_qp, 1e4737_qp, &
        1e4738_qp, 1e4739_qp, 1e4740_qp, 1e4741_qp, 1e4742_qp, 1e4743_qp, &
        1e4744_qp, 1e4745_qp, 1e4746_qp, 1e4747_qp, 1e4748_qp, 1e4749_qp, &
        1e4750_qp, 1e4751_qp, 1e4752_qp, 1e4753_qp, 1e4754_qp, 1e4755_qp, &
        1e4756_qp, 1e4757_qp, 1e4758_qp, 1e4759_qp, 1e4760_qp, 1e4761_qp, &
        1e4762_qp, 1e4763_qp, 1e4764_qp, 1e4765_qp, 1e4766_qp, 1e4767_qp, &
        1e4768_qp, 1e4769_qp, 1e4770_qp, 1e4771_qp, 1e4772_qp, 1e4773_qp, &
        1e4774_qp, 1e4775_qp, 1e4776_qp, 1e4777_qp, 1e4778_qp, 1e4779_qp, &
        1e4780_qp, 1e4781_qp, 1e4782_qp, 1e4783_qp, 1e4784_qp, 1e4785_qp, &
        1e4786_qp, 1e4787_qp, 1e4788_qp, 1e4789_qp, 1e4790_qp, 1e4791_qp, &
        1e4792_qp, 1e4793_qp, 1e4794_qp, 1e4795_qp, 1e4796_qp, 1e4797_qp, &
        1e4798_qp, 1e4799_qp, 1e4800_qp, 1e4801_qp, 1e4802_qp, 1e4803_qp, &
        1e4804_qp, 1e4805_qp, 1e4806_qp, 1e4807_qp, 1e4808_qp, 1e4809_qp, &
        1e4810_qp, 1e4811_qp, 1e4812_qp, 1e4813_qp, 1e4814_qp, 1e4815_qp, &
        1e4816_qp, 1e4817_qp, 1e4818_qp, 1e4819_qp, 1e4820_qp, 1e4821_qp, &
        1e4822_qp, 1e4823_qp, 1e4824_qp, 1e4825_qp, 1e4826_qp, 1e4827_qp, &
        1e4828_qp, 1e4829_qp, 1e4830_qp, 1e4831_qp, 1e4832_qp, 1e4833_qp, &
        1e4834_qp, 1e4835_qp, 1e4836_qp, 1e4837_qp, 1e4838_qp, 1e4839_qp, &
        1e4840_qp, 1e4841_qp, 1e4842_qp, 1e4843_qp, 1e4844_qp, 1e4845_qp, &
        1e4846_qp, 1e4847_qp, 1e4848_qp, 1e4849_qp, 1e4850_qp, 1e4851_qp, &
        1e4852_qp, 1e4853_qp, 1e4854_qp, 1e4855_qp, 1e4856_qp, 1e4857_qp, &
        1e4858_qp, 1e4859_qp, 1e4860_qp, 1e4861_qp, 1e4862_qp, 1e4863_qp, &
        1e4864_qp, 1e4865_qp, 1e4866_qp, 1e4867_qp, 1e4868_qp, 1e4869_qp, &
        1e4870_qp, 1e4871_qp, 1e4872_qp, 1e4873_qp, 1e4874_qp, 1e4875_qp, &
        1e4876_qp, 1e4877_qp, 1e4878_qp, 1e4879_qp, 1e4880_qp, 1e4881_qp, &
        1e4882_qp, 1e4883_qp, 1e4884_qp, 1e4885_qp, 1e4886_qp, 1e4887_qp, &
        1e4888_qp, 1e4889_qp, 1e4890_qp, 1e4891_qp, 1e4892_qp, 1e4893_qp, &
        1e4894_qp, 1e4895_qp, 1e4896_qp, 1e4897_qp, 1e4898_qp, 1e4899_qp, &
        1e4900_qp, 1e4901_qp, 1e4902_qp, 1e4903_qp, 1e4904_qp, 1e4905_qp, &
        1e4906_qp, 1e4907_qp, 1e4908_qp, 1e4909_qp, 1e4910_qp, 1e4911_qp, &
        1e4912_qp, 1e4913_qp, 1e4914_qp, 1e4915_qp, 1e4916_qp, 1e4917_qp, &
        1e4918_qp, 1e4919_qp, 1e4920_qp, 1e4921_qp, 1e4922_qp, 1e4923_qp, &
        1e4924_qp, 1e4925_qp, 1e4926_qp, 1e4927_qp, 1e4928_qp, 1e4929_qp, &
        1e4930_qp, 1e4931_qp, 1e4932_qp, &
        REAL_QP_INF &
    ]
    real(QP), parameter :: REAL_QP_DEC_POWERS(-4967:4933) = [ &
        REAL_QP_DEC_POWERS1, &
        REAL_QP_DEC_POWERS2, &
        REAL_QP_DEC_POWERS3, &
        REAL_QP_DEC_POWERS4, &
        REAL_QP_DEC_POWERS5, &
        REAL_QP_DEC_POWERS6, &
        REAL_QP_DEC_POWERS7 &
    ]
#endif

contains

#define _FILE "../math/dec_digits.inc"
#define _ID _INTEGER
#include "../inc/defs.inc"
#undef _FILE

#define _FILE "../math/dec_exponent.inc"
#define _ID _REAL
#include "../inc/defs.inc"
#undef _FILE

end module stdlib_math
